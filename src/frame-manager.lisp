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
    ((frame climi::standard-application-frame) &rest keys)
  "Transparent runner bootstrap for render-stack applications.

Specialised on STANDARD-APPLICATION-FRAME (more specific than
APPLICATION-FRAME) so that McCLIM's own :around on APPLICATION-FRAME
remains in the call chain and is invoked via CALL-NEXT-METHOD.

Execution order in the CLOS chain:
  1. This method (standard-application-frame :around) — bootstrap
  2. McCLIM's :around (application-frame :around) — binds
     *APPLICATION-FRAME*, does adoption, enable, runs the event loop
  3. Primary method (application-frame) — calls top-level lambda

At this point the frame has no port (adoption happens inside
call-next-method), so we identify render-stack frames by checking
*DEFAULT-SERVER-PATH*.

Passes through immediately when:
  - *DEFAULT-SERVER-PATH* is not :RENDER-STACK (other backends).
  - *RUNNER* is already bound (mixin, expert, or bootstrap in progress).

Otherwise sets *RUNNER* and bootstraps depending on the calling thread:

  Case A — OS main thread (standalone app, sbcl --load):
    Spawns a background thread that calls call-next-method (which runs
    McCLIM's :around: adoption, port creation, event loop). RUNNER-RUN
    blocks this thread. Port creation finds *RUNNER* bound and dispatches
    %port-init-sdl3 via submit-to-main-thread.

  Case B — worker thread (SLIME/Emacs, async evaluation):
    Starts runner on main thread via run-runner-on-main-thread, then
    calls call-next-method here. Port creation dispatches %port-init-sdl3
    to the main thread. Event loop runs on this thread.

In both cases RUN-FRAME-TOP-LEVEL returns when the event loop exits."
  (declare (ignore keys))
  ;; Pass-through: not a render-stack application.
  (unless (and (listp clim:*default-server-path*)
               (eq (first clim:*default-server-path*) :render-stack))
    (return-from clim:run-frame-top-level (call-next-method)))
  (let ((existing-runner rs-internals:*runner*))
    (cond
      ;; Runner is already bound — mixin, expert, or bootstrap in progress.
      ;; (Don't check state: runner may be :stopped during the race window
      ;;  between make-thread and runner-run setting state to :running.)
      (existing-runner
       (call-next-method))

      ;; No runner — transparent lazy bootstrap.
      ;; Set *RUNNER* before call-next-method so that port creation
      ;; (inside McCLIM's :around, triggered by adoption) finds *RUNNER*
      ;; bound and calls %port-init-sdl3 from initialize-instance :after.
      (t
       (let ((new-runner (make-instance 'rs-internals:main-thread-runner)))
         (setf rs-internals:*runner* new-runner)
         (if (eq (bt2:current-thread) (rs-internals:find-main-thread))
             ;; Case A: on main thread.
             ;; Bg thread runs call-next-method (McCLIM's :around + event loop).
             ;; Main thread blocks in runner-run.
             (%bootstrap-runner-main-thread new-runner
                                            (lambda () (call-next-method)))
             ;; Case B: on worker thread (e.g., SLIME).
             ;; Start runner on main thread first, then call-next-method here.
             (%bootstrap-runner-worker-thread new-runner
                                              (lambda () (call-next-method)))))))))

;;; ============================================================================
;;; Initialization
;;; ============================================================================

(defun initialize-render-stack ()
  "Initialize the render-stack backend.
   Call this before using the :render-stack server path."
  (log:info "render-stack backend initialized"))
