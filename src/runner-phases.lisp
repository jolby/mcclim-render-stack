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
  ((delegate :initarg :delegate
             :reader clim-event-drain-phase-delegate
             :type multi-window-render-delegate
             :documentation
             "The MULTI-WINDOW-RENDER-DELEGATE that owns event routing.
Calls DRAIN-SDL3-EVENTS which polls SDL3, translates events, and
distributes them to McCLIM sheet queues via DISTRIBUTE-EVENT."))
  (:documentation
   "Runner phase that drains SDL3 events through the McCLIM delegate.

Each iteration calls DRAIN-SDL3-EVENTS on the delegate, which polls
the SDL3 event queue, translates raw events into McCLIM event objects,
and routes them to the appropriate sheet queues.

Configure with a time budget to prevent event bursts from starving
the render phase:
  (make-clim-event-drain-phase delegate :time-budget-ms 4.0)"))

(defmethod rs-internals:run-phase ((phase clim-event-drain-phase) runner)
  (declare (ignore runner))
  (drain-sdl3-events (clim-event-drain-phase-delegate phase)))

(defun make-clim-event-drain-phase (delegate &key (time-budget-ms 4.0))
  "Create a CLIM-EVENT-DRAIN-PHASE for DELEGATE.

Arguments:
  DELEGATE       — a MULTI-WINDOW-RENDER-DELEGATE instance
  TIME-BUDGET-MS — milliseconds to spend draining events per iteration (default 4ms)"
  (make-instance 'clim-event-drain-phase
                 :name :drain-clim-events
                 :time-budget-itu (rs-internals:itu-from-milliseconds time-budget-ms)
                 :delegate delegate))

;;; -----------------------------------------------------------------------
;;; CLIM render phase
;;; -----------------------------------------------------------------------

(defclass clim-render-phase (rs-internals:runner-phase)
  ((engine   :initarg :engine
             :reader clim-render-phase-engine
             :documentation "The RENDER-ENGINE instance to consume frames from.")
   (delegate :initarg :delegate
             :reader clim-render-phase-delegate
             :documentation "The MULTI-WINDOW-RENDER-DELEGATE for rasterization."))
  (:documentation
   "Runner phase that does non-blocking consume-and-draw for McCLIM.

Each iteration attempts a non-blocking PIPELINE-TRY-CONSUME on the
engine's pipeline. When a frame is ready, calls RENDER-DELEGATE-DRAW
on the delegate. Errors during drawing are logged, not propagated.

No time budget — we always render the frame we have."))

(defmethod rs-internals:run-phase ((phase clim-render-phase) runner)
  (declare (ignore runner))
  (multiple-value-bind (item got-it)
      (pipeline-try-consume
       (render-engine-pipeline (clim-render-phase-engine phase)))
    (when got-it
      (format *error-output* "~&[DIAG] clim-render-phase: consumed frame from pipeline~%")
      (handler-case
          (render-delegate-draw (clim-render-phase-delegate phase) item)
        (error (e)
          (format *error-output* "~&[DIAG] Error in delegate-draw: ~A~%" e)
          (log:debug :clim-render-phase "Error in delegate-draw: ~A~%" e)
          (log:error :mcclim-render-stack "Error in delegate-draw: ~A" e))))))

(defun make-clim-render-phase (engine delegate)
  "Create a CLIM-RENDER-PHASE for ENGINE and DELEGATE.

Arguments:
  ENGINE   — a RENDER-ENGINE instance (owns the pipeline)
  DELEGATE — a MULTI-WINDOW-RENDER-DELEGATE for rasterization"
  (make-instance 'clim-render-phase
                 :name :render-clim-frames
                 :engine engine
                 :delegate delegate))
