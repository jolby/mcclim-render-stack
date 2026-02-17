;;;; port.lisp — McCLIM Port using render-stack (SDL3 + Impeller)
;;;;
;;;; The port is the top-level connection to the display server.
;;;; This implementation uses SDL3 for windowing and events,
;;;; and Flutter Impeller (via render-stack) for rendering.
;;;;
;;;; CRITICAL ARCHITECTURE:
;;;; - NO custom event queue - Uses McCLIM's concurrent-queue via distribute-event
;;;; - Port owns main-thread-loop - Drains SDL3 events and renders
;;;; - process-next-event is minimal stub - Events come via distribute-event
;;;; - Event flow: SDL3 → drain-sdl3-events → distribute-event → sheet queue

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
   ;; REMOVED: event-thread (events handled on main thread)
   (quit-requested :accessor port-quit-requested :initform nil)
   (typography-context :accessor port-typography-context :initform nil
                       :documentation "Impeller typography context for text rendering.")
   ;; REMOVED: event-queue, event-queue-lock, event-queue-condition
   ;; Events now flow through McCLIM's concurrent-queue via distribute-event
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
- Main Thread: SDL3/GL operations, drain-sdl3-events, rendering
- UI Thread: McCLIM event loop, process-next-event
- Frame Thread: render-stack engine (drives begin-frame/end-frame)

Event Flow:
1. Main thread polls SDL3 via drain-sdl3-events in main-thread-loop
2. Events translated and routed via distribute-event to sheet's concurrent-queue
3. Frame thread wakes via condition-notify, calls begin-frame
4. UI thread processes events from queue via process-next-event"))

;;; ============================================================================
;;; Port Initialization
;;; ============================================================================

(defun %init-port-on-main-thread (port)
  "Initialize SDL3 and create the host on the main thread.
   Window, delegate, and engine are created later in realize-mirror."
  ;; Initialize SDL3 video
  (rs-sdl3:init-sdl3-video)
  ;; Create host (no window yet — deferred to realize-mirror)
  (setf (port-host port) (make-instance 'rs-sdl3:sdl3-host)))

(defmethod initialize-instance :after ((port render-stack-port) &key)
  "Initialize port. Does NOT create window/engine - deferred to realize-mirror.
   
   Thread Contract: Can be called from any thread. Creates background thread for SDL3."
  
  ;; Initialize typography context (doesn't need SDL3)
  (setf (port-typography-context port) (frs:make-typography-context))
  
  ;; Assert multiprocessing mode (required for concurrent-queue)
  (assert clim-sys:*multiprocessing-p* ()
          "render-stack-port requires multiprocessing (concurrent-queue)")
  
  ;; Start SDL3 operations in a dedicated thread
  ;; SDL3 requires all operations (init, events, GL) on the same thread
  (bt:make-thread
   (lambda ()
     ;; Register this thread as the main thread for SDL3/GL operations
     (rs-internals:register-main-thread)
     ;; Initialize SDL3 on this thread
     (rs-sdl3:init-sdl3-video)
     ;; Create host
     (setf (port-host port) (make-instance 'rs-sdl3:sdl3-host))
     ;; Initialize global engine
     (initialize-global-engine)
     ;; Run the main event/render loop (this blocks until port-quit-requested)
     (main-thread-loop port))
   :name (format nil "mcclim-rs-main-~A" (gensym)))
  
  ;; Wait a bit for the background thread to initialize
  ;; This ensures host and engine are ready before returning
  (loop repeat 100
        until (and (port-host port) mcclim-render-stack::*global-engine-initialized*)
        do (sleep 0.01))
  
  ;; Verify initialization succeeded
  (unless (port-host port)
    (error "Failed to initialize SDL3 host - timeout waiting for background thread")))

;;; ============================================================================
;;; Main Thread Loop (CRITICAL - replaces custom queue + event thread)
;;; ============================================================================

(defun main-thread-loop (port)
  "Main thread event+render loop.
   Runs on main thread via trivial-main-thread. Responsible for:
   1. Draining SDL3 events into McCLIM sheet queues (EVERY iteration)
   2. Non-blocking pipeline consume + rasterization (when frames ready)
   3. Yielding via SDL_WaitEventTimeout (OS-level, no busy-spin)
   
   Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread main-thread-loop)
  
  (let ((engine *global-engine*)
        (delegate *global-delegate*))
    (loop until (port-quit-requested port) do
      ;; 1. ALWAYS drain SDL3 events → distribute to McCLIM sheet queues
      ;; This wakes frame threads via concurrent-queue condition-notify
      (drain-sdl3-events delegate)
      
      ;; 2. Non-blocking consume: rasterize if a frame is ready
      ;; Use pipeline-try-consume (NOT pipeline-consume which blocks)
      (multiple-value-bind (item got-it)
          (render-stack:pipeline-try-consume 
           (render-stack:render-engine-pipeline engine))
        (when got-it
          (handler-case
              (render-stack:render-delegate-draw delegate item)
            (error (e) 
              (log:error :mcclim-render-stack "Error in delegate-draw: ~A" e)))))
      
      ;; 3. Yield to OS — wakes on SDL event or timeout
      ;; Uses OS-level blocking (epoll/kqueue), not busy-wait
      (rs-sdl3:wait-event-timeout 1))))  ; 1ms timeout

;;; ============================================================================
;;; REMOVED: Custom Event Queue Operations
;;; ============================================================================
;;;
;;; The following have been REMOVED in favor of McCLIM's concurrent-queue:
;;; - port-enqueue-event (use distribute-event instead)
;;; - port-dequeue-event (events come via sheet's concurrent-queue)
;;; - port-peek-event (not needed)
;;; - port-event-queue-empty-p (not needed)
;;; - start-event-thread (events handled on main thread)
;;; - event-loop (replaced by main-thread-loop)
;;;
;;; Event translation functions moved to multi-window-delegate.lisp:
;;; - translate-sdl3-event
;;; - sdl3-modifiers-to-clim
;;; - sdl3-keycode-to-key-name
;;; - sdl3-keycode-to-character

;;; ============================================================================
;;; Port Protocol Methods
;;; ============================================================================

(defmethod destroy-port ((port render-stack-port))
  "Clean up all resources."
  ;; Signal quit
  (setf (port-quit-requested port) t)
  
  ;; REMOVED: No event thread to wait for (events on main thread now)
  
  ;; Unregister from global delegate if registered
  (when (and *global-delegate* (port-window-id port))
    (unregister-port-from-delegate *global-delegate* port))
  
  ;; Stop and destroy engine (only if this port created it - but it's global now)
  ;; Global engine is shut down via shutdown-global-engine, not here
  
  ;; Clean up delegate (only if this port created it - but it's global now)
  ;; Global delegate is cleaned up with engine
  
  ;; Release typography context
  (when (port-typography-context port)
    (frs:release-typography-context (port-typography-context port))
    (setf (port-typography-context port) nil))

  ;; Destroy window
  (when (port-window port)
    (rs-host:destroy-window (port-host port) (port-window port))
    (setf (port-window port) nil))
  
  ;; Quit SDL3
  (rs-sdl3:quit-sdl3))

(defmethod process-next-event ((port render-stack-port) 
                               &key wait-function (timeout 0.016))
  "Process-next-event for render-stack port.
   
   In our architecture, the main thread pushes events via distribute-event
   from the main-thread-loop. Frame threads consume from per-sheet
   concurrent-queues. This method satisfies the McCLIM protocol contract.
   
   Returns: (values NIL :wait-function) if wait-function fires,
            (values NIL :timeout) otherwise.
   
   Thread Contract: MUST be called on UI thread."
  (rs-internals:assert-ui-thread process-next-event)
  
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

;; REMOVED: get-next-event method - McCLIM only requires process-next-event

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
   
   Thread Contract: Called on UI thread. SDL3 window creation dispatched to main thread."
  (clim:with-bounding-rectangle* (x y :width w :height h) sheet
    (let* ((host (port-host port))
           ;; Create window on main thread
           (window (trivial-main-thread:call-in-main-thread
                    (lambda ()
                      (rs-host:make-window host
                                          :title (climi::sheet-pretty-name sheet)
                                          :width (floor w)
                                          :height (floor h)
                                          :x (floor x)
                                          :y (floor y)))))
           ;; Get SDL3 window ID for event routing
           (window-id (trivial-main-thread:call-in-main-thread
                       (lambda ()
                         (%sdl3:get-window-id 
                          (rs-sdl3::sdl3-window-handle window))))))
      
      ;; Store on port
      (setf (port-window port) window
            (port-gl-context port) (rs-host:window-graphics-context window)
            (port-window-id port) window-id)
      
      ;; Register with global delegate
      (register-port-with-delegate *global-delegate* port window-id)
      
      ;; Store sheet in window table for event lookup
      (setf (gethash window (port-window-table port)) sheet)
      
      ;; Create mirror
      (make-instance 'render-stack-mirror
                     :sdl-window window))))

(defmethod destroy-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  "Destroy mirror and unregister from global delegate."
  (let ((mirror (climi::sheet-direct-mirror sheet)))
    (when mirror
      (let ((window (mirror-sdl-window mirror)))
        (when window
          ;; Remove from window table
          (remhash window (port-window-table port))
          
          ;; Unregister from delegate
          (unregister-port-from-delegate *global-delegate* port)
          
          ;; Destroy window on main thread
          (trivial-main-thread:call-in-main-thread
           (lambda ()
             (rs-host:destroy-window (port-host port) window))))
        
        ;; Clear mirror from sheet
        (setf (climi::sheet-direct-mirror sheet) nil)))))

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
;;; Backwards Compatibility
;;; ============================================================================

;; These functions are deprecated but kept for compatibility during transition
(defun port-enqueue-event (port event)
  "DEPRECATED: Events now use distribute-event. Stub for compatibility."
  (declare (ignore port event))
  (warn "port-enqueue-event is deprecated - use distribute-event instead"))

(defun port-dequeue-event (port &optional timeout)
  "DEPRECATED: Events now come via concurrent-queue. Stub for compatibility."
  (declare (ignore port timeout))
  nil)
