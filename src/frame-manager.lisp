;;;; frame-manager.lisp — Frame Manager for McCLIM on render-stack
;;;;
;;;; The frame manager handles adoption/disowning of application frames.
;;;; It works with the render-stack port to manage frame lifecycles.
;;;;
;;;; Also defines RENDER-STACK-FRAME-MIXIN — include this in application
;;;; frame classes to get automatic runner bootstrap when run-frame-top-level
;;;; is called. Without it, the application must pre-bind *runner* manually.

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; Frame Manager
;;; ============================================================================

(defclass render-stack-frame-manager (climi::standard-frame-manager)
  ((active-frames :initform 0
                  :accessor render-stack-frame-manager-active-frames
                  :documentation "Reference count of adopted frames")
   (frame-lock :initform (bt2:make-lock "render-stack-frame-manager")
               :reader render-stack-frame-manager-lock))
  (:documentation "Frame manager for render-stack backend."))

(defmethod climi::adopt-frame :after
    ((fm render-stack-frame-manager) (frame climi::standard-application-frame))
  "Track frame adoption."
  (bt2:with-lock-held ((render-stack-frame-manager-lock fm))
    (incf (render-stack-frame-manager-active-frames fm))))

(defmethod climi::disown-frame :after
    ((fm render-stack-frame-manager) (frame climi::standard-application-frame))
  "Track frame disowning."
  (bt2:with-lock-held ((render-stack-frame-manager-lock fm))
    (decf (render-stack-frame-manager-active-frames fm))))

;;; ============================================================================
;;; Render Stack Frame Mixin
;;; ============================================================================

(defclass render-stack-frame-mixin ()
  ()
  (:documentation
   "Mixin for CLIM application frames using the render-stack backend.

Include in your frame class to enable automatic runner bootstrap:

  (clim:define-application-frame my-app (render-stack-frame-mixin)
    ...)

When RUN-FRAME-TOP-LEVEL is called, this mixin arranges for the
render-stack main-thread-runner to own the OS main thread, with the
CLIM event loop in the appropriate thread.

Two startup modes are handled transparently:

  Case A — called from the OS main thread (standalone app, sbcl --load):
    The runner loop is started on this thread. The CLIM event loop runs
    in a spawned background thread. RUN-FRAME-TOP-LEVEL returns when the
    CLIM event loop exits (which also stops the runner).

  Case B — called from a worker thread (SLIME/Emacs, async evaluation):
    The OS main thread is interrupted and starts the runner loop there.
    The CLIM event loop continues on the calling (worker) thread.
    RUN-FRAME-TOP-LEVEL returns when the CLIM event loop exits.

In both cases, port initialization dispatches SDL3/GL work to the main
thread via the runner's task queue, so the OS main thread constraint is
satisfied without any additional application-level plumbing.

If RS-INTERNALS:*RUNNER* is already bound and :RUNNING when
RUN-FRAME-TOP-LEVEL is called, the mixin does nothing (expert mode)."))

;;; -----------------------------------------------------------------------
;;; Bootstrap helpers (internal, not exported)
;;; -----------------------------------------------------------------------

(defun %bootstrap-runner-main-thread (runner clim-thunk)
  "Case A bootstrap: move CLIM event loop to background, runner-run on main thread.

Spawns a background thread that calls CLIM-THUNK (which runs adoption and
the CLIM event loop). When CLIM-THUNK returns, the background thread calls
RUNNER-STOP so that RUNNER-RUN on the main thread exits.

Blocks the calling (main) thread inside RUNNER-RUN until the app exits."
  (let ((bg-error nil))
    (bt2:make-thread
     (lambda ()
       (let ((rs-internals:*runner* runner))
         (unwind-protect
              (handler-case
                  (funcall clim-thunk)
                (error (e)
                  (setf bg-error e)))
           ;; Always stop the runner when the CLIM event loop exits.
           (rs-internals:runner-stop runner))))
     :name "CLIM Event Loop")
    ;; Block the main thread. Returns when runner-stop is called.
    (unwind-protect
         (rs-internals:runner-run runner)
      ;; Clear the global binding when the runner exits.
      (setf rs-internals:*runner* nil))
    (when bg-error
      (error bg-error))))

(defun %bootstrap-runner-worker-thread (runner clim-thunk)
  "Case B bootstrap: claim OS main thread for the runner, CLIM event loop stays here.

Interrupts the OS main thread to start RUNNER-RUN there. Blocks until the
runner is live (task queue accepting submissions), then calls CLIM-THUNK
on this (worker) thread. When CLIM-THUNK returns, the runner is stopped."
  ;; Interrupt main thread and start runner-run there.
  ;; Blocks until runner is in :running state (semaphore signaled inside runner-run).
  (rs-internals:run-runner-on-main-thread runner)
  ;; Runner is live. Run the CLIM event loop on this worker thread.
  (unwind-protect
       (let ((rs-internals:*runner* runner))
         (funcall clim-thunk))
    ;; Stop the runner when the CLIM event loop exits.
    (rs-internals:runner-stop runner)
    ;; Clear the global binding.
    (setf rs-internals:*runner* nil)))

;;; -----------------------------------------------------------------------
;;; run-frame-top-level :around — automatic runner bootstrap
;;; -----------------------------------------------------------------------

(defmethod clim:run-frame-top-level :around
    ((frame render-stack-frame-mixin) &rest keys)
  "Automatic runner bootstrap for render-stack backend.

If RS-INTERNALS:*RUNNER* is already :RUNNING, delegates immediately
(expert/application-managed mode).

If RS-INTERNALS:*RUNNER* is NIL, creates a new runner and bootstraps
it depending on the calling thread:
  - OS main thread → Case A: runner-run blocks here, CLIM loop in background
  - Worker thread  → Case B: runner-run started on main thread, CLIM loop here"
  (declare (ignore keys))
  (let ((existing-runner rs-internals:*runner*))
    (cond
      ;; Expert mode: runner already active — do nothing special.
      ((and existing-runner
            (member (rs-internals:runner-state existing-runner)
                    '(:starting :running)))
       (call-next-method))

      ;; Lazy bootstrap: no runner yet.
      ((null existing-runner)
       (let ((new-runner (make-instance 'rs-internals:main-thread-runner)))
         ;; Set globally so port-init (which runs in the spawned bg thread)
         ;; can access *runner* for task submission.
         (setf rs-internals:*runner* new-runner)
         (if (eq (bt2:current-thread) (rs-internals:find-main-thread))
             ;; Case A: called from the OS main thread.
             (%bootstrap-runner-main-thread new-runner
                                           (lambda () (call-next-method)))
             ;; Case B: called from a worker thread (e.g., SLIME).
             (%bootstrap-runner-worker-thread new-runner
                                             (lambda () (call-next-method))))))

      ;; Runner exists but is stopped/stopping — don't touch it (expert cleanup).
      (t
       (call-next-method)))))

;;; ============================================================================
;;; Initialization
;;; ============================================================================

(defun initialize-render-stack ()
  "Initialize the render-stack backend.
   Call this before using the :render-stack server path."
  (log:info "render-stack backend initialized"))
