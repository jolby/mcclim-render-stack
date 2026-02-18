;;;; frame-manager.lisp — Frame Manager for McCLIM on render-stack
;;;;
;;;; The frame manager handles adoption/disowning of application frames.
;;;; It works with the render-stack port to manage frame lifecycles.
;;;;
;;;; Transparent bootstrap: a run-frame-top-level :around on application-frame
;;;; lazily starts the main-thread-runner when needed. After setting
;;;;   (setf clim:*default-server-path* '(:render-stack))
;;;; any standard CLIM application (run-frame-top-level ...) works without
;;;; any frame class modification.
;;;;
;;;; RENDER-STACK-FRAME-MIXIN remains available as an explicit alternative
;;;; for applications that want to declare the backend dependency in the
;;;; frame class, or for expert runners that pre-bind *runner* before
;;;; run-frame-top-level is called.

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
   "Optional mixin for CLIM application frames using the render-stack backend.

NOTE: As of the transparent bootstrap refactor, this mixin is no longer
required. Standard CLIM applications work without it — see the
run-frame-top-level :around on application-frame below.

Use this mixin when you want to explicitly declare the backend dependency
in your frame class, or when the runner must be bootstrapped BEFORE frame
adoption (e.g., expert runners that pre-configure phases):

  (clim:define-application-frame my-app (render-stack-frame-mixin)
    ...)

When RUN-FRAME-TOP-LEVEL is called, this mixin sets *RUNNER* before
standard-application-frame :around runs (before adoption/port creation),
so SDL3 init happens during initialize-instance :after rather than being
deferred. The transparent application-frame :around detects the runner is
already active and passes through.

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

;;; -----------------------------------------------------------------------
;;; run-frame-top-level :around — transparent bootstrap (no mixin required)
;;; -----------------------------------------------------------------------

(defmethod clim:run-frame-top-level :around
    ((frame clim:application-frame) &rest keys)
  "Transparent runner bootstrap for any render-stack application frame.

Fires AFTER standard-application-frame :around (which does adoption and
port creation), so the port exists when this method executes.

Passes through immediately when:
  - The port is not a render-stack port (other backends, tests).
  - *RUNNER* is already active (mixin or expert already bootstrapped it).

Otherwise lazily bootstraps the runner, depending on the calling thread:

  Case A — OS main thread (standalone app, sbcl --load):
    SDL3 initialized directly here (we ARE the main thread).
    Event loop spawned in a background thread.
    RUNNER-RUN blocks this thread until the app exits.

  Case B — worker thread (SLIME/Emacs, async evaluation):
    Runner started on OS main thread via RUN-RUNNER-ON-MAIN-THREAD.
    SDL3 initialized via SUBMIT-TO-MAIN-THREAD (runner is live by then).
    Event loop runs on this (worker) thread.

In both cases RUN-FRAME-TOP-LEVEL returns when the event loop exits."
  (declare (ignore keys))
  ;; Pass-through: not a render-stack port (other backends, unit tests, etc).
  (unless (typep (clim:frame-port frame) 'render-stack-port)
    (return-from clim:run-frame-top-level (call-next-method)))
  (let ((existing-runner rs-internals:*runner*))
    (cond
      ;; Runner already active — mixin or expert bootstrapped it; pass through.
      ((and existing-runner
            (member (rs-internals:runner-state existing-runner)
                    '(:starting :running)))
       (call-next-method))

      ;; No runner — transparent lazy bootstrap.
      ((null existing-runner)
       (let ((new-runner (make-instance 'rs-internals:main-thread-runner))
             (port (clim:frame-port frame)))
         (setf rs-internals:*runner* new-runner)
         (if (eq (bt2:current-thread) (rs-internals:find-main-thread))
             ;; Case A: on OS main thread.
             ;; Call %port-init-sdl3 directly (we are the main thread).
             ;; Then trampoline: event loop in background, runner-run blocks here.
             (progn
               (%port-init-sdl3 port)
               (%bootstrap-runner-main-thread new-runner
                                             (lambda () (call-next-method))))
             ;; Case B: on a worker thread (e.g., SLIME).
             ;; Start runner on main thread first (makes submit-to-main-thread safe),
             ;; then dispatch SDL3 init via submit, then run event loop here.
             (%bootstrap-runner-worker-thread new-runner
                                             (lambda ()
                                               (%port-init-sdl3 port)
                                               (call-next-method))))))

      ;; Runner exists but not active — pass through (expert cleanup path).
      (t
       (call-next-method)))))

;;; ============================================================================
;;; Initialization
;;; ============================================================================

(defun initialize-render-stack ()
  "Initialize the render-stack backend.
   Call this before using the :render-stack server path."
  (log:info "render-stack backend initialized"))
