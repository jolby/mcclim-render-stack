;;;; mcclim-render-stack/src/runner-phases.lisp
;;;; McCLIM-specific runner phases for the main-thread-runner system.
;;;;
;;;; Replaces the hand-written main-thread-loop in port.lisp with
;;;; composable, testable phases that plug into a main-thread-runner.

(in-package :mcclim-render-stack)

;;; -----------------------------------------------------------------------
;;; CLIM event drain phase
;;; -----------------------------------------------------------------------

(defclass clim-event-drain-phase (rs-internals:budgeted-runner-phase)
  ((port :initarg :port
         :reader clim-event-drain-phase-port
         :type render-stack-port
         :documentation
         "The RENDER-STACK-PORT managing the runtime.
Drains SDL3 events and distributes them to McCLIM sheet queues.")
   (iteration-count :accessor clim-event-drain-phase-iteration-count
                    :initform 0
                    :documentation "Runner loop iteration counter for diagnostics."))
  (:documentation
   "Runner phase that drains SDL3 events through the McCLIM port.

Each iteration calls DRAIN-SDL3-EVENTS, which polls the SDL3 event queue,
translates raw events into McCLIM event objects, and routes them to the
appropriate sheet queues via the port's window registry.

Configure with a time budget to prevent event bursts from starving
the render phase:
  (make-clim-event-drain-phase port :time-budget-ms 4.0)"))

(defmethod rs-internals:run-phase ((phase clim-event-drain-phase) runner)
  (let ((port (clim-event-drain-phase-port phase))
        (n    (incf (clim-event-drain-phase-iteration-count phase))))
    ;; Periodic heartbeat: confirms the runner loop is alive and shows speed.
    (when (zerop (mod n 30))
      (format *error-output* "~&[RUNNER] iter=~D state=~A~%"
              n (rs-internals:runner-state runner)))
    (drain-sdl3-events-for-port port)
    ;; Safety net: if quit was requested but frame-exit hasn't been called after
    ;; the event drain, forcefully stop the runner.
    (when (port-quit-requested port)
      (log:info :mcclim-render-stack "Quit requested, stopping runner")
      (rs-internals:runner-stop runner))))

(defun drain-sdl3-events-for-port (port)
  "Poll SDL3 events and route them through the port.
Translates raw SDL3 events into McCLIM events and distributes them
via DISTRIBUTE-EVENT to McCLIM's per-sheet concurrent-queue.

Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread drain-sdl3-events-for-port)
  (rs-sdl3:with-sdl3-event (ev)
    (loop while (rs-sdl3:poll-event ev)
          do (let* ((event-type (rs-sdl3:get-event-type ev))
                    (window-id  (get-window-id-from-sdl3-event ev event-type)))
               ;; Log every event except noisy mouse-motion.
               (unless (member event-type '(:mouse-motion :mouse-wheel))
                 (format *error-output* "~&[EVENT] type=~A win=~A~%"
                         event-type window-id))
               (cond
                 ;; Global quit event -- no window-id
                 ((eq event-type :quit)
                  (format *error-output* "~&[EVENT] :quit received — notifying all sheets~%")
                  (setf (port-quit-requested port) t)
                  ;; Distribute window-manager-delete-event to all sheets so
                  ;; McCLIM's event loop can call frame-exit and exit cleanly.
                  ;; (SDL3 on some platforms sends :quit directly without a
                  ;;  preceding :window-close-requested.)
                  ;; Snapshot sheets under lock, distribute without lock held.
                  (let ((sheets (bt2:with-lock-held ((port-registry-lock port))
                                  (let ((s nil))
                                    (maphash (lambda (id mirror)
                                               (declare (ignore id))
                                               (push (mirror-sheet mirror) s))
                                             (port-window-registry port))
                                    s))))
                    (dolist (sheet sheets)
                      (distribute-event
                       port
                       (make-instance 'window-manager-delete-event
                                      :sheet sheet)))))
                 ;; Window-specific event -- route via mirror registry
                 (window-id
                  (let ((sheet (find-sheet-by-window-id port window-id)))
                    (cond
                      (sheet
                       (let ((clim-event (translate-sdl3-event port ev)))
                         (if clim-event
                             (progn
                               (format *error-output* "~&[EVENT] distributing ~A to sheet ~A~%"
                                       (type-of clim-event) sheet)
                               (distribute-event port clim-event))
                             (unless (member event-type '(:mouse-motion :mouse-wheel))
                               (format *error-output* "~&[EVENT] no CLIM translation for ~A~%"
                                       event-type)))))
                      (t
                       (format *error-output* "~&[EVENT] no sheet for win=~A (type=~A)~%"
                               window-id event-type))))))))))

(defun make-clim-event-drain-phase (port &key (time-budget-ms 4.0))
  "Create a CLIM-EVENT-DRAIN-PHASE for PORT.

Arguments:
   PORT           -- a RENDER-STACK-PORT instance
   TIME-BUDGET-MS -- milliseconds to spend draining events per iteration (default 4ms)"
  (make-instance 'clim-event-drain-phase
                 :name :drain-clim-events
                 :time-budget-itu (rs-internals:itu-from-milliseconds time-budget-ms)
                 :port port))

;;; -----------------------------------------------------------------------
;;; CLIM render phase
;;; -----------------------------------------------------------------------

(defclass clim-render-phase (rs-internals:runner-phase)
  ((port :initarg :port
         :reader clim-render-phase-port
         :type render-stack-port
         :documentation "The RENDER-STACK-PORT managing the engine and runtime.")
   (last-render-time :accessor clim-render-phase-last-render-time
                     :initform 0
                     :documentation "Internal-real-time of last fallback render, for throttling."))
  (:documentation
   "Runner phase that does non-blocking consume-and-draw for McCLIM.

Each iteration attempts a non-blocking PIPELINE-TRY-CONSUME on the
engine's pipeline. When a frame is ready, calls RENDER-DELEGATE-DRAW
on the runtime (which IS the delegate). Errors during drawing are
logged, not propagated.

No time budget -- we always render the frame we have."))

(defmethod rs-internals:run-phase ((phase clim-render-phase) runner)
  (declare (ignore runner))
  (let* ((port    (clim-render-phase-port phase))
         (runtime (port-runtime port))
         (engine  (runtime-engine runtime)))
    (multiple-value-bind (item got-it)
        (pipeline-try-consume (render-engine-pipeline engine))
      (cond
        ;; Normal pipeline path: frame was produced by render-engine-tick on the UI thread.
        (got-it
         (format *error-output* "~&[DIAG] clim-render-phase: consumed frame from pipeline~%")
         (handler-case
             (render-delegate-draw runtime item)
           (error (e)
             (format *error-output* "~&[DIAG] Error in delegate-draw: ~A~%" e)
             (log:debug  :clim-render-phase "Error in delegate-draw: ~A~%" e)
             (log:error :mcclim-render-stack "Error in delegate-draw: ~A" e))))

        ;; Fallback: direct render when pipeline is empty, throttled to ~30fps.
        ;; McCLIM's event loop may not call process-next-event in all paths
        ;; (it can block directly on the concurrent-queue via distribute-event),
        ;; so render-engine-tick may never be driven from the UI side.
        ;; This fallback renders directly without the pipeline so we can confirm
        ;; the Impeller path works end-to-end.
        (t
         (let ((impeller-ctx  (runtime-impeller-context runtime))
               (registry-size (bt2:with-lock-held ((port-registry-lock port))
                                (hash-table-count (port-window-registry port))))
               (now           (get-internal-real-time))
               (min-interval  (floor internal-time-units-per-second 30)))
           (when (and impeller-ctx (plusp registry-size)
                      (> (- now (clim-render-phase-last-render-time phase))
                         min-interval))
             (setf (clim-render-phase-last-render-time phase) now)
             (handler-case
                 (render-delegate-draw runtime nil)
               (error (e)
                 (format *error-output* "~&[ERROR] clim-render-phase: direct render failed: ~A~%" e)
                 (log:error :mcclim-render-stack "Error in direct render: ~A" e))))))))))

(defun make-clim-render-phase (port)
  "Create a CLIM-RENDER-PHASE for PORT.

Arguments:
   PORT -- a RENDER-STACK-PORT instance (runtime must be initialized)"
  (make-instance 'clim-render-phase
                 :name :render-clim-frames
                 :port port))
