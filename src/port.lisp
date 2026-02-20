;;;; port.lisp — McCLIM Port using render-stack (SDL3 + Impeller)
;;;;
;;;; The port is the top-level connection to the display server.
;;;; This implementation uses SDL3 for windowing and events,
;;;; and Flutter Impeller (via render-stack) for rendering.
;;;;
;;;; CRITICAL ARCHITECTURE:
;;;; - NO custom event queue — Uses McCLIM's concurrent-queue via distribute-event
;;;; - Runner phases handle event drain + rendering on the OS main thread
;;;; - process-next-event is a minimal stub — events come via distribute-event
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
  ((runtime
    :accessor port-runtime
    :initform (make-instance 'render-stack-runtime)
    :documentation "The render-stack-runtime managing the engine and Impeller context.")

   (host
    :accessor port-host
    :initform nil)

   (window-registry
    :accessor port-window-registry
    :initform (make-hash-table :test 'eql)
    :documentation "Maps SDL3 window-id (integer) → render-stack-mirror.
All access must hold port-registry-lock.")

   (registry-lock
    :accessor port-registry-lock
    :initform (bt2:make-lock :name "port-registry")
    :documentation "Protects window-registry.")

   (%keyboard-focus
    :accessor port-%keyboard-focus
    :initform nil
    :documentation "CLIM sheet with keyboard focus, or NIL for default.")

   (quit-requested
    :accessor port-quit-requested
    :initform nil)

   (modifier-state
    :accessor port-modifier-state
    :initform 0
    :documentation "Current keyboard modifier state bitmask."))
  (:documentation "McCLIM port using render-stack (SDL3 + Impeller).

Threading Model:
- Main Thread (OS): Runner phases drain SDL3 events and rasterize frames
- UI/App Thread: McCLIM event loop, process-next-event, drawing

REQUIREMENT: rs-internals:*runner* must be bound before port creation.
Use rs-internals:with-runner at application startup.

Event Flow:
1. Runner's clim-event-drain-phase polls SDL3 via drain-sdl3-events
2. Events translated and routed via distribute-event to sheet's concurrent-queue
3. McCLIM frame loop wakes from per-sheet queue condition-notify"))

;;; ============================================================================
;;; Port Initialization
;;; ============================================================================

(defmethod initialize-instance :after ((port render-stack-port) &key)
  "Initialize port.  Window/engine creation is deferred to realize-mirror.

REQUIREMENT: rs-internals:*runner* must be bound (application must have
started a main-thread-runner before creating the port).

Thread Contract: Can be called from any thread.  Runtime initialization
is dispatched to the main thread via the runner's task queue."

  ;; Register this thread as the UI/CLIM event loop thread.
  (unless rs-internals:*ui-thread*
    (rs-internals:register-ui-thread))

  ;; Require multiprocessing (concurrent-queue needs it).
  (assert clim-sys:*multiprocessing-p* ()
          "render-stack-port requires multiprocessing (concurrent-queue)")

  ;; Fail fast if no runner is active.
  (unless rs-internals:*runner*
    (error "render-stack-port requires an active rs-internals:*runner*. ~
            Start the application with rs-internals:with-runner or ~
            rs-internals:claim-main-thread before creating the port."))

  ;; Initialize runtime (engine, typography context, SDL3 host) on the main thread.
  (if (rs-internals:runner-main-thread-p rs-internals:*runner*)
      (initialize-runtime (port-runtime port) port)
      (rs-internals:submit-to-main-thread rs-internals:*runner*
        (lambda ()
          (initialize-runtime (port-runtime port) port))
        :blocking t
        :tag :mcclim-port-init))

  ;; Inject CLIM runner phases if the runner was started without phases
  ;; (transparent bootstrap case).
  (when (and rs-internals:*runner*
             (null (rs-internals:runner-phases rs-internals:*runner*)))
    (rs-internals:submit-to-main-thread rs-internals:*runner*
      (lambda ()
        (setf (rs-internals:runner-phases rs-internals:*runner*)
              (make-clim-runner-phases port)))
      :blocking t
      :tag :inject-clim-runner-phases)))

;;; ============================================================================
;;; Port Protocol Methods
;;; ============================================================================

(defmethod destroy-port ((port render-stack-port))
  "Clean up port resources.

Does NOT stop the runner, shut down the engine, or quit SDL3 —
those are application-level concerns managed at startup/shutdown."
  (setf (port-quit-requested port) t)
  (log:debug :port "destroy-port: quit requested")

  ;; Destroy all remaining mirrors.
  ;; Collect under lock, then destroy without holding it.
  (let ((mirrors (bt2:with-lock-held ((port-registry-lock port))
                   (let ((m nil))
                     (maphash (lambda (id mirror)
                                (declare (ignore id))
                                (push mirror m))
                              (port-window-registry port))
                     m))))
    (dolist (mirror mirrors)
      (let ((window (mirror-sdl-window mirror)))
        (when window
          (rs-sdl3:destroy-sdl3-window window)))
      (deregister-mirror port mirror))))

(defmethod process-next-event ((port render-stack-port)
                               &key wait-function (timeout 0.016))
  "Minimal process-next-event stub for render-stack-port.

In our architecture, runner phases push events via distribute-event from
clim-event-drain-phase on the main thread.  This method satisfies the
McCLIM protocol contract but does no event polling itself.

Returns: (values NIL :wait-function) if wait-function fires,
         (values NIL :timeout) otherwise."
  (format *error-output* "~&[DIAG] process-next-event: ENTERED~%")

  (when (port-quit-requested port)
    (return-from process-next-event nil))

  (when (and wait-function (funcall wait-function))
    (return-from process-next-event (values nil :wait-function)))

  (when timeout
    (sleep (min timeout 0.01)))

  (values nil :timeout))

(defmethod port-force-output ((port render-stack-port))
  "Flush pending drawing operations."
  ;; The render engine handles this automatically.
  )

(defmethod port-set-mirror-region ((port render-stack-port) mirror region)
  "Set the mirror's visible region."
  (declare (ignore mirror region)))

(defmethod port-set-mirror-transformation ((port render-stack-port) mirror transformation)
  "Set the mirror's transformation."
  (declare (ignore mirror transformation)))

;;; ============================================================================
;;; Port Capabilities
;;; ============================================================================

(defmethod port-keyboard-input-focus ((port render-stack-port))
  "Return the sheet with keyboard focus.

Prefers the explicitly-set focus; falls back to the sheet of the first
mirror in the registry (for single-window applications)."
  (or (port-%keyboard-focus port)
      ;; Fallback: first mirror's sheet
      (bt2:with-lock-held ((port-registry-lock port))
        (let ((sheet nil))
          (maphash (lambda (id mirror)
                     (declare (ignore id))
                     (setf sheet (mirror-sheet mirror)))
                   (port-window-registry port))
          sheet))))

(defmethod (setf port-keyboard-input-focus) (sheet (port render-stack-port))
  "Set the sheet with keyboard focus."
  (setf (port-%keyboard-focus port) sheet))

(defmethod port-pointer :before ((port render-stack-port))
  "Ensure the pointer is created when first accessed."
  (unless (slot-value port 'pointer)
    (setf (slot-value port 'pointer)
          (make-instance 'render-stack-pointer :port port))))

;;; ============================================================================
;;; McCLIM Mirror Protocol
;;; ============================================================================
;;;
;;; Defined here (not mirror.lisp) so render-stack-port exists as a class
;;; before these methods are compiled.

(defmethod realize-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  "Create a render-stack-mirror for SHEET and register it with PORT.

Computes the initial window size from the sheet's bounding rectangle,
falling back to frame-geometry* or 500×400 if layout has not completed.
SDL3 window creation and GL context access are dispatched to the main thread.

Thread Contract: Called on UI thread. SDL3/GL ops dispatched via runner."
  (clim:with-bounding-rectangle* (x y :width w :height h) sheet
    (declare (ignore x y))
    ;; At realize-mirror time layout may not have run — bounding rect is often 0×0.
    ;; frame-geometry* gives the user-requested size.
    (let* ((frame   (ignore-errors (clim:pane-frame sheet)))
           (frame-w nil)
           (frame-h nil)
           (_ign    (when frame
                      (ignore-errors
                        (multiple-value-setq (frame-w frame-h)
                          (climi::frame-geometry* frame)))))
           (width   (or (and (plusp w) (floor w))
                        (and frame-w (floor frame-w))
                        500))
           (height  (or (and (plusp h) (floor h))
                        (and frame-h (floor frame-h))
                        400))
           ;; make-sdl3-window transparently dispatches to the main thread.
           (window  (rs-sdl3:make-sdl3-window
                     (or (clime:sheet-pretty-name sheet) "(McCLIM)")
                     width height))
           ;; SDL3 window ID is needed for event routing; fetch on main thread.
           (window-id (rs-internals:submit-to-main-thread rs-internals:*runner*
                        (lambda ()
                          (%sdl3:get-window-id
                           (rs-sdl3::sdl3-window-handle window)))
                        :blocking t
                        :tag :get-window-id))
           (mirror  (make-instance 'render-stack-mirror
                                   :sdl-window window
                                   :window-id  window-id
                                   :gl-context (rs-sdl3::sdl3-window-gl-context window)
                                   :port       port
                                   :sheet      sheet
                                   :width      width
                                   :height     height)))
      ;; Register so event routing can find the mirror immediately.
      (register-mirror port mirror)

      ;; Create the Impeller GL context now that an SDL3 GL context is current.
      ;; Idempotent — skipped if already created (e.g. second window).
      (rs-internals:submit-to-main-thread rs-internals:*runner*
        (lambda ()
          (initialize-runtime-impeller-context (port-runtime port)))
        :blocking t
        :tag :create-impeller-context)

      ;; Return the mirror — McCLIM stores it as sheet-direct-mirror.
      mirror)))

(defmethod destroy-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  "Destroy SHEET's mirror, releasing its Impeller surface and SDL3 window.

Thread Contract: Called on UI thread. Impeller/SDL3 cleanup on main thread."
  (let ((mirror (climi::sheet-direct-mirror sheet)))
    (when mirror
      ;; Release cached Impeller surface on the main thread.
      (when (mirror-surface mirror)
        (rs-internals:submit-to-main-thread rs-internals:*runner*
          (lambda ()
            (rs-internals:without-float-traps
              (frs:release-surface (mirror-surface mirror)))
            (setf (mirror-surface mirror) nil))
          :blocking t
          :tag :release-mirror-surface))

      ;; Remove from registry so event routing stops finding this mirror.
      (deregister-mirror port mirror)

      ;; Destroy the SDL3 window (transparently dispatches to main thread).
      (let ((window (mirror-sdl-window mirror)))
        (when window
          (rs-sdl3:destroy-sdl3-window window)))

      ;; Clear the mirror reference from the sheet.
      (setf (climi::sheet-direct-mirror sheet) nil))))

;;; ============================================================================
;;; Runner Phase Construction
;;; ============================================================================

(defun make-clim-runner-phases (port &key (event-budget-ms 4.0) (yield-timeout-ms 16))
  "Create the list of runner phases for a McCLIM main-thread-runner.

Phase order:
1. CLIM event drain — polls SDL3 events, routes to McCLIM sheet queues
2. CLIM render — non-blocking pipeline consume + delegate draw
3. Event wait yield — OS-level blocking until next event or timeout

Arguments:
  PORT             — a RENDER-STACK-PORT instance (runtime must be initialized)
  EVENT-BUDGET-MS  — ms budget for event draining per iteration (default 4.0)
  YIELD-TIMEOUT-MS — ms to wait for events between iterations (default 16)"
  (unless (and port (runtime-initialized-p (port-runtime port)))
    (error "make-clim-runner-phases requires a port with initialized runtime. ~
            Check that port initialize-instance completed successfully."))
  (list (make-clim-event-drain-phase port :time-budget-ms event-budget-ms)
        (make-clim-render-phase port)
        (rs-sdl3:make-event-wait-yield-phase :timeout-ms yield-timeout-ms)))
