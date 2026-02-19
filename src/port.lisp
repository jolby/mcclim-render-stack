;;;; port.lisp — McCLIM Port using render-stack (SDL3 + Impeller)
;;;;
;;;; The port is the top-level connection to the display server.
;;;; This implementation uses SDL3 for windowing and events,
;;;; and Flutter Impeller (via render-stack) for rendering.
;;;;
;;;; CRITICAL ARCHITECTURE:
;;;; - NO custom event queue — Uses McCLIM's concurrent-queue via distribute-event
;;;; - Runner phases handle event drain + rendering on the OS main thread
;;;; - process-next-event is minimal stub — Events come via distribute-event
;;;; - Event flow: SDL3 → drain-sdl3-events → distribute-event → sheet queue
;;;;
;;;; THREADING REQUIREMENT:
;;;; The application MUST start a main-thread-runner (rs-internals:with-runner
;;;; or rs-internals:claim-main-thread) BEFORE creating a port. The port uses
;;;; rs-internals:*runner* to dispatch SDL3/GL work to the OS main thread.
;;;; See runner-phases.lisp for the McCLIM-specific phases.

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; Port Class
;;; ============================================================================

(defclass render-stack-port (basic-port)
  ((runtime :accessor port-runtime
            :initform (make-instance 'render-stack-runtime)
            :documentation "The render-stack-runtime instance managing engine, context, and window registry.")
   
   (host :accessor port-host :initform nil)
   
   (window :accessor port-window :initform nil)
   
   (window-id :accessor port-window-id
              :initform nil
              :documentation "SDL3 window ID for event routing.")
   
   (%keyboard-focus :accessor port-%keyboard-focus :initform nil
                    :documentation "CLIM sheet with keyboard focus (or NIL for default).")
   
   (gl-context :accessor port-gl-context :initform nil)
   
   (quit-requested :accessor port-quit-requested :initform nil)
   
   (typography-context :accessor port-typography-context :initform nil
                       :documentation "Impeller typography context for text rendering. (Deprecated: use runtime-typography-context)")
   
   (needs-redraw-p :accessor port-needs-redraw-p
                   :initform t
                   :documentation "T if port needs a redraw.")
   
   (modifier-state :accessor port-modifier-state :initform 0
                   :documentation "Current keyboard modifier state.")
   
   (window-table :accessor port-window-table
                 :initform (make-hash-table :test 'eq)
                 :documentation "SDL3 window → CLIM sheet mapping for event dispatch."))
  (:documentation "McCLIM port using render-stack (SDL3 + Impeller).

Threading Model:
- Main Thread (OS): Runner phases drain SDL3 events and rasterize frames
- UI/App Thread: McCLIM event loop, process-next-event, drawing
- Frame Thread: render-stack engine (drives begin-frame/end-frame)

REQUIREMENT: rs-internals:*runner* must be bound before port creation.
Use rs-internals:with-runner at application startup.

Event Flow:
1. Runner's clim-event-drain-phase polls SDL3 via drain-sdl3-events
2. Events translated and routed via distribute-event to sheet's concurrent-queue
3. Frame thread wakes via condition-notify, calls begin-frame
4. UI thread processes events from queue via process-next-event"))

;;; ============================================================================
;;; Port Initialization
;;; ============================================================================

(defmethod initialize-instance :after ((port render-stack-port) &key)
  "Initialize port. Does NOT create window/engine — deferred to realize-mirror.
    
    REQUIREMENT: rs-internals:*runner* must be bound (application must have
    started a main-thread-runner before creating the port).
    
    Thread Contract: Can be called from any thread. Runtime initialization is
    dispatched to the main thread via the runner's task queue."
   
   ;; Register this thread as the UI/CLIM event loop thread.
   ;; Port creation always happens on the UI thread (the bg thread in Case A,
   ;; the worker thread in Case B). Required for assert-ui-thread in begin-frame.
   (unless rs-internals:*ui-thread*
     (rs-internals:register-ui-thread))

   ;; Assert multiprocessing mode (required for concurrent-queue)
   (assert clim-sys:*multiprocessing-p* ()
           "render-stack-port requires multiprocessing (concurrent-queue)")
   
   ;; Require an active runner — fail fast with a clear error
   (unless rs-internals:*runner*
     (error "render-stack-port requires an active rs-internals:*runner*. ~
             Start the application with rs-internals:with-runner or ~
             rs-internals:claim-main-thread before creating the port."))
   
   ;; Initialize runtime (engine, typography context, SDL3 host) on the main thread.
   ;; If we're already on the main thread, call directly (avoids deadlock).
   ;; Otherwise, dispatch via the runner's task queue (blocking).
   (if (rs-internals:runner-main-thread-p rs-internals:*runner*)
       (initialize-runtime (port-runtime port) port)
       (rs-internals:submit-to-main-thread rs-internals:*runner*
         (lambda ()
           (initialize-runtime (port-runtime port) port))
         :blocking t
         :tag :mcclim-port-init))

   ;; After runtime initialization, inject the CLIM runner phases if the runner
   ;; was started without phases (transparent bootstrap case). Expert runners
   ;; that pre-configure their own phases are left untouched.
   (when (and rs-internals:*runner*
              (null (rs-internals:runner-phases rs-internals:*runner*)))
     (rs-internals:submit-to-main-thread rs-internals:*runner*
       (lambda ()
         (setf (rs-internals:runner-phases rs-internals:*runner*)
               (make-clim-runner-phases port)))
       :blocking t
       :tag :inject-clim-runner-phases)))

;;; ============================================================================
;;; See runner-phases.lisp: clim-event-drain-phase + clim-render-phase.
;;; The runner yield phase (rs-sdl3:make-event-wait-yield-phase) handles
;;; OS-level event waiting between iterations.
;;; ============================================================================

;;; ============================================================================
;;; Port Protocol Methods
;;; ============================================================================

(defmethod destroy-port ((port render-stack-port))
  "Clean up port resources.
    
    Does NOT stop the runner, shut down the engine, or quit SDL3 —
    those are application-level concerns managed at startup/shutdown."
  ;; Signal quit
  (setf (port-quit-requested port) t)
  
  (log:debug :port "Destroy port called. Set quit requested")
  
  ;; Unregister window from runtime registry
  (when (port-window-id port)
    (unregister-window (port-runtime port) (port-window-id port)))

  ;; Destroy window on the main thread via runner
  (when (port-window port)
    (let ((window (port-window port)))
      (rs-sdl3:destroy-sdl3-window window))
    (setf (port-window port) nil)))

(defmethod process-next-event ((port render-stack-port)
                               &key wait-function (timeout 0.016))
  "Process-next-event for render-stack port.

    In our architecture, runner phases push events via distribute-event
    from the clim-event-drain-phase. Frame threads consume from per-sheet
    concurrent-queues. This method satisfies the McCLIM protocol contract.

    Returns: (values NIL :wait-function) if wait-function fires,
             (values NIL :timeout) otherwise."

   ;; Diagnostic: confirm whether McCLIM's event loop ever calls this method
   (format *error-output* "~&[DIAG] process-next-event: ENTERED~%")

   ;; Check quit flag
   (when (port-quit-requested port)
     (return-from process-next-event nil))
   
   ;; Check wait-function first (per CLX pattern)
   (when (and wait-function (funcall wait-function))
     (return-from process-next-event (values nil :wait-function)))
   
   ;; Signal engine if this port needs a frame
   ;; This is non-blocking - just sets a flag in the engine
   (when (port-needs-redraw-p port)
     (format *error-output* "~&[DIAG] process-next-event: port-needs-redraw, requesting frame~%")
     (render-stack:render-engine-request-frame (runtime-engine (port-runtime port)))
     ;; Mark sheet as dirty in runtime
     (mark-sheet-dirty (port-runtime port) (port-find-sheet-for-window port (port-window port)))
     ;; Clear local flag
     (setf (port-needs-redraw-p port) nil))

   ;; Drive the engine tick when a frame is pending.
   ;; render-engine-tick calls begin-frame (builds display lists)
   ;; then pipeline-produce to hand the frame to the main thread for rasterization.
   ;; Must be called on the UI thread (asserted inside begin-frame).
   (let ((engine (runtime-engine (port-runtime port))))
     (when (and engine
                (render-stack:render-engine-running-p engine)
                (render-stack:frame-clock-frame-pending-p
                 (render-stack:render-engine-clock engine)))
       (format *error-output* "~&[DIAG] process-next-event: calling render-engine-tick~%")
       (render-stack:render-engine-tick engine)))

   ;; Events arrive asynchronously from main thread.
   ;; Briefly yield to allow event delivery.
   ;; The concurrent-queue's condition-wait is the real blocking mechanism.
   (when timeout
     (sleep (min timeout 0.01)))
   
   ;; Return protocol per McCLIM spec
   (values nil :timeout))

(defmethod port-force-output ((port render-stack-port))
  "Flush pending drawing operations."
  ;; The render engine handles this automatically
  )

(defmethod port-set-mirror-region ((port render-stack-port) mirror region)
  "Set the mirror's visible region."
  (declare (ignore mirror region)))

(defmethod port-set-mirror-transformation ((port render-stack-port) mirror transformation)
  "Set the mirror's transformation."
  (declare (ignore mirror transformation)))

;;; ============================================================================
;;; Mirror Protocol
;;; ============================================================================

(defmethod realize-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  "Create mirror for sheet and register with runtime.
    
    Thread Contract: Called on UI thread. SDL3 window creation dispatched
    to the main thread via define-main-thread-op (transparent dispatch)."
   (clim:with-bounding-rectangle* (x y :width w :height h) sheet
     (let* (;; At realize-mirror time, layout hasn't completed yet, so bounding rect may be 0×0.
            ;; Use frame-geometry* to get the user-requested size, fall back to 500×400.
            (frame (ignore-errors (clim:pane-frame sheet)))
            (frame-w nil) (frame-h nil)
            (_ign (when frame
                    (ignore-errors
                      (multiple-value-setq (frame-w frame-h)
                        (climi::frame-geometry* frame)))))
            (width (or (and (plusp w) (floor w))
                       (and frame-w (floor frame-w))
                       500))
            (height (or (and (plusp h) (floor h))
                        (and frame-h (floor frame-h))
                        400))
            ;; Create window — rs-sdl3:make-sdl3-window auto-dispatches to main thread
            (window (rs-sdl3:make-sdl3-window
                     (or (clime:sheet-pretty-name sheet) "(McCLIM)")
                     width height))
            ;; Get SDL3 window ID for event routing (main thread call via runner)
            (window-id (rs-internals:submit-to-main-thread rs-internals:*runner*
                         (lambda ()
                           (%sdl3:get-window-id
                            (rs-sdl3::sdl3-window-handle window)))
                         :blocking t
                         :tag :get-window-id)))
       
       ;; Store on port
       (setf (port-window port) window
             (port-gl-context port) (rs-sdl3::sdl3-window-gl-context window)
             (port-window-id port) window-id)

       ;; Create the Impeller GL context on the main thread now that an
       ;; SDL3 GL context is current. Idempotent — skipped for subsequent windows.
       (rs-internals:submit-to-main-thread rs-internals:*runner*
         (lambda ()
           (initialize-runtime-impeller-context (port-runtime port)))
         :blocking t
         :tag :create-impeller-context)

       ;; Register window with runtime
       (register-window (port-runtime port) window-id sheet)

       ;; Store sheet in window table for event lookup
       (setf (gethash window (port-window-table port)) sheet)

       ;; Create mirror
       (make-instance 'render-stack-mirror
                      :sdl-window window))))

(defmethod destroy-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  "Destroy mirror and unregister from runtime.
    
    Thread Contract: Called on UI thread. SDL3 window destruction dispatched
    to the main thread via define-main-thread-op (transparent dispatch)."
   (let ((mirror (climi::sheet-direct-mirror sheet)))
     (when mirror
       (let ((window (mirror-sdl-window mirror)))
         (when window
           ;; Remove from window table
           (remhash window (port-window-table port))
           
           ;; Unregister from runtime
           (unregister-window (port-runtime port) (port-window-id port))
           
           ;; Destroy window — auto-dispatches to main thread
           (rs-sdl3:destroy-sdl3-window window)))
       
       ;; Clear mirror from sheet
       (setf (climi::sheet-direct-mirror sheet) nil))))

;;; ============================================================================
;;; Port Capabilities
;;; ============================================================================

(defun port-find-sheet-for-window (port window)
  "Look up the CLIM sheet associated with an SDL3 window."
  (gethash window (port-window-table port)))

(defmethod port-keyboard-input-focus ((port render-stack-port))
  "Return the sheet with keyboard focus.
  
  Prefers the explicitly-set focus, falling back to the first registered window's sheet."
   (or (port-%keyboard-focus port)
       ;; Fall back to first registered window from runtime registry
       (let ((runtime (port-runtime port)))
         (bt2:with-lock-held ((runtime-registry-lock runtime))
           (let ((sheet nil))
             (maphash (lambda (id s) (declare (ignore id)) (setf sheet s))
                      (runtime-window-registry runtime))
             sheet)))))

(defmethod (setf port-keyboard-input-focus) (sheet (port render-stack-port))
  "Set the sheet with keyboard focus.
  
  McCLIM calls this when focus changes."
   (setf (port-%keyboard-focus port) sheet))

(defmethod port-pointer :before ((port render-stack-port))
  "Ensure the pointer is created when accessed."
  (unless (slot-value port 'pointer)
    (setf (slot-value port 'pointer)
          (make-instance 'render-stack-pointer :port port))))

;;; ============================================================================
;;; Application Startup Helper
;;; ============================================================================

(defun start-mcclim-render-stack (&key (event-budget-ms 4.0) (yield-timeout-ms 16))
  "Start a main-thread-runner configured for McCLIM.
   
   This function MUST be called from the OS main thread (or via
   rs-internals:claim-main-thread). It:
   1. Creates runner phases for CLIM event drain, rendering, and yielding
   2. Starts the runner on the main thread
   3. Returns a function that starts the CLIM application in a background thread
   
   Usage:
     (rs-internals:claim-main-thread
       (lambda ()
         (start-mcclim-render-stack)))
   
   Or for applications that need to run code in the background thread:
     (rs-internals:claim-main-thread
       (lambda ()
         (rs-internals:with-runner (runner
             :phases (make-clim-runner-phases))
           ;; Body runs in background thread
           (my-clim-app))))
   
   Arguments:
     EVENT-BUDGET-MS  — ms budget for event draining per iteration (default 4.0)
     YIELD-TIMEOUT-MS — ms to wait for events between iterations (default 16)"
  (declare (ignore event-budget-ms yield-timeout-ms))
  ;; Placeholder for now — the phases need *global-engine* and *global-delegate*
  ;; which are created during port init. See make-clim-runner-phases.
  (error "start-mcclim-render-stack: Use rs-internals:with-runner with ~
          make-clim-runner-phases after global engine init. ~
          See runner-phases.lisp for details."))

(defun make-clim-runner-phases (port &key (event-budget-ms 4.0) (yield-timeout-ms 16))
  "Create the list of runner phases for a McCLIM main-thread-runner.
    
    PORT: The render-stack-port instance (runtime must be initialized)
    
    Phase order:
    1. CLIM event drain — polls SDL3 events, routes to McCLIM sheet queues
    2. CLIM render — non-blocking pipeline consume + delegate draw
    3. Event wait yield — OS-level blocking until next event or timeout
    
    Arguments:
      PORT — The render-stack-port managing the runtime
      EVENT-BUDGET-MS  — ms budget for event draining per iteration (default 4.0)
      YIELD-TIMEOUT-MS — ms to wait for events between iterations (default 16)"
   (unless (and port (runtime-initialized-p (port-runtime port)))
     (error "make-clim-runner-phases requires a port with initialized runtime. ~
             Check that port initialize-instance completed successfully."))
   (let ((runtime (port-runtime port)))
     (list (make-clim-event-drain-phase port :time-budget-ms event-budget-ms)
           (make-clim-render-phase port)
           (rs-sdl3:make-event-wait-yield-phase :timeout-ms yield-timeout-ms))))
