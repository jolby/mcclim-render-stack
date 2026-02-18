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
;;;; SDL3/GL operations must run on the OS main thread.
;;;; The port handles this automatically: if *runner* is already bound at
;;;; port-creation time (expert or frame-mixin mode), SDL3 is initialized
;;;; immediately. Otherwise SDL3 init is deferred to the run-frame-top-level
;;;; :around hook on application-frame, which lazily bootstraps the runner.
;;;; See runner-phases.lisp for the McCLIM-specific runner phases.

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; Port Class
;;; ============================================================================

(defclass render-stack-port (basic-port)
  ((host :accessor port-host :initform nil)
   (window :accessor port-window :initform nil)
   (window-id :accessor port-window-id
              :initform nil
              :documentation "SDL3 window ID for event routing.")
   (engine :accessor port-engine :initform nil)
   (delegate :accessor port-delegate :initform nil)
   (gl-context :accessor port-gl-context :initform nil)
   (quit-requested :accessor port-quit-requested :initform nil)
   (typography-context :accessor port-typography-context :initform nil
                       :documentation "Impeller typography context for text rendering.")
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

SDL3 init is deferred if *runner* is not bound at creation time — the
run-frame-top-level :around hook on application-frame bootstraps the
runner and finishes initialization transparently.

Event Flow:
1. Runner's clim-event-drain-phase polls SDL3 via drain-sdl3-events
2. Events translated and routed via distribute-event to sheet's concurrent-queue
3. Frame thread wakes via condition-notify, calls begin-frame
4. UI thread processes events from queue via process-next-event"))

;;; ============================================================================
;;; Port Initialization
;;; ============================================================================

(defun %port-init-sdl3 (port)
  "Initialize SDL3, the global host, and the global engine on the main thread.

Idempotent: skips if PORT-HOST is already set.

Thread dispatch:
  - If called from the OS main thread: executes directly (handles the lazy
    bootstrap Case A where runner-run has not yet started).
  - If called from another thread: dispatches via SUBMIT-TO-MAIN-THREAD
    (blocking). Requires *RUNNER* to be bound and running.

After engine init, installs the standard runner phases if the runner has
no phases yet (lazy bootstrap). Expert runners that pre-configure phases
are left untouched."
  (unless (port-host port)
    (flet ((do-init ()
             ;; Ensure the main-thread registry is populated.
             ;; In lazy bootstrap Case A the runner hasn't started yet,
             ;; so register-main-thread hasn't been called yet.
             (unless rs-internals:*main-thread*
               (rs-internals:register-main-thread))
             (rs-sdl3:init-sdl3-video)
             (setf (port-host port) (make-instance 'rs-sdl3:sdl3-host))
             (initialize-global-engine)))
      (if (eq (bt2:current-thread) (rs-internals:find-main-thread))
          (do-init)
          (rs-internals:submit-to-main-thread rs-internals:*runner*
            #'do-init :blocking t :tag :mcclim-port-init)))
    ;; Wire phases if the runner has none (lazy bootstrap; experts set their own).
    (let ((runner rs-internals:*runner*))
      (when (and runner (null (rs-internals:runner-phases runner)))
        (setf (rs-internals:runner-phases runner)
              (make-clim-runner-phases))))))

(defmethod initialize-instance :after ((port render-stack-port) &key)
  "Initialize port. Does NOT create the window/engine — deferred to realize-mirror.

Two modes:
  Expert / frame-mixin: *runner* is bound at creation time → SDL3 initialized now.
  Lazy bootstrap:       *runner* is nil → SDL3 deferred to run-frame-top-level :around.

Thread Contract: Can be called from any thread."
  ;; Typography context is SDL3-independent — always create it immediately.
  (setf (port-typography-context port) (frs:make-typography-context))

  ;; Multiprocessing is required for McCLIM's concurrent-queue.
  (assert clim-sys:*multiprocessing-p* ()
          "render-stack-port requires multiprocessing (concurrent-queue)")

  ;; Expert / frame-mixin mode: runner already bound → initialize SDL3 now.
  ;; Lazy bootstrap mode: runner is nil → SDL3 deferred (see frame-manager.lisp).
  (when rs-internals:*runner*
    (%port-init-sdl3 port)))

;;; ============================================================================
;;; NOTE: main-thread-loop has been replaced by runner phases.
;;; See runner-phases.lisp: clim-event-drain-phase + clim-render-phase.
;;; The runner yield phase (rs-sdl3:make-event-wait-yield-phase) handles
;;; OS-level event waiting between iterations.
;;; ============================================================================

;;; ============================================================================
;;; restart-port — override to suppress McCLIM's default I/O thread
;;; ============================================================================

(defmethod restart-port ((port render-stack-port))
  "No-op override.

McCLIM's default restart-port spawns a background thread running
process-next-event in a loop (the port I/O thread). We do not want
that — our runner phases on the OS main thread handle all SDL3 event
polling and rendering. Events reach McCLIM via distribute-event called
from CLIM-EVENT-DRAIN-PHASE.")

;;; ============================================================================
;;; Port Protocol Methods
;;; ============================================================================

(defmethod destroy-port ((port render-stack-port))
  "Clean up port resources.
   
   Does NOT stop the runner, shut down the global engine, or quit SDL3 —
   those are application-level concerns managed at startup/shutdown."
  ;; Signal quit
  (setf (port-quit-requested port) t)
  
  ;; Unregister from global delegate if registered
  (when (and *global-delegate* (port-window-id port))
    (unregister-port-from-delegate *global-delegate* port))
  
  ;; Release typography context
  (when (port-typography-context port)
    (frs:release-typography-context (port-typography-context port))
    (setf (port-typography-context port) nil))

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
  
  ;; Check quit flag
  (when (port-quit-requested port)
    (return-from process-next-event nil))
  
  ;; Check wait-function first (per CLX pattern)
  (when (and wait-function (funcall wait-function))
    (return-from process-next-event (values nil :wait-function)))
  
  ;; Signal engine if this port needs a frame
  ;; This is non-blocking - just sets a flag in the engine
  (when (port-needs-redraw-p port)
    (render-stack:render-engine-request-frame *global-engine*)
    ;; Mark port as dirty in delegate
    (bt2:with-lock-held ((delegate-dirty-lock *global-delegate*))
      (unless (member port (delegate-dirty-ports *global-delegate*))
        (push port (delegate-dirty-ports *global-delegate*))))
    ;; Clear local flag
    (setf (port-needs-redraw-p port) nil))
  
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
  "Create mirror for sheet and register with global delegate.
   
   Thread Contract: Called on UI thread. SDL3 window creation dispatched
   to the main thread via define-main-thread-op (transparent dispatch)."
  (clim:with-bounding-rectangle* (x y :width w :height h) sheet
    (let* (;; Create window — rs-sdl3:make-sdl3-window auto-dispatches to main thread
           (window (rs-sdl3:make-sdl3-window
                    (or (climi::sheet-pretty-name sheet) "(McCLIM)")
                    (floor w) (floor h)
                    :x (floor x) :y (floor y)))
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
      
      ;; Register with global delegate
      (register-port-with-delegate *global-delegate* port window-id)
      
      ;; Store sheet in window table for event lookup
      (setf (gethash window (port-window-table port)) sheet)
      
      ;; Create mirror
      (make-instance 'render-stack-mirror
                     :sdl-window window))))

(defmethod destroy-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  "Destroy mirror and unregister from global delegate.
   
   Thread Contract: Called on UI thread. SDL3 window destruction dispatched
   to the main thread via define-main-thread-op (transparent dispatch)."
  (let ((mirror (climi::sheet-direct-mirror sheet)))
    (when mirror
      (let ((window (mirror-sdl-window mirror)))
        (when window
          ;; Remove from window table
          (remhash window (port-window-table port))
          
          ;; Unregister from delegate
          (unregister-port-from-delegate *global-delegate* port)
          
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
  "Return the sheet with keyboard focus."
  (let ((window (port-window port)))
    (when window
      (port-find-sheet-for-window port window))))

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

(defun make-clim-runner-phases (&key (event-budget-ms 4.0) (yield-timeout-ms 16))
  "Create the list of runner phases for a McCLIM main-thread-runner.
   
   REQUIREMENT: *global-engine* and *global-delegate* must be initialized
   before calling this (they are set up during port initialization).
   
   Phase order:
   1. CLIM event drain — polls SDL3 events, routes to McCLIM sheet queues
   2. CLIM render — non-blocking pipeline consume + delegate draw
   3. Event wait yield — OS-level blocking until next event or timeout
   
   Arguments:
     EVENT-BUDGET-MS  — ms budget for event draining per iteration (default 4.0)
     YIELD-TIMEOUT-MS — ms to wait for events between iterations (default 16)"
  (unless (and *global-engine* *global-delegate*)
    (error "make-clim-runner-phases requires initialized global engine and delegate. ~
            Create a port first, then call this."))
  (list (make-clim-event-drain-phase *global-delegate*
                                      :time-budget-ms event-budget-ms)
        (make-clim-render-phase *global-engine* *global-delegate*)
        (rs-sdl3:make-event-wait-yield-phase :timeout-ms yield-timeout-ms)))
