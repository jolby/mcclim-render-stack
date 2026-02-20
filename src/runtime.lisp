;;;; runtime.lisp — render-stack-runtime class and initialization
;;;;
;;;; Consolidates the render-stack engine and Impeller context into a single
;;;; CLOS object stored on the port.  The runtime IS the render-delegate.
;;;;
;;;; Window management is now on the port (port-window-registry) and the
;;;; mirror (render-stack-mirror).  The runtime holds a back-pointer to the
;;;; port so it can iterate registered mirrors during rendering.
;;;;
;;;; Thread Safety Model:
;;;;   engine            — read-only after init, safe from any thread
;;;;   impeller-context  — read-only after init, safe from any thread
;;;;   typography-context— read-only after init, safe from any thread
;;;;   port              — read-only after initialize-runtime, safe from any thread

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; Debug Helpers
;;; ============================================================================

(defvar *debug-frame-limit* 30
  "Exit after this many rendered frames. Useful during development to avoid
kill -9. Set to NIL to disable (run indefinitely).")

;;; ============================================================================
;;; GL Proc Address Callback for Impeller
;;; ============================================================================
;;;
;;; Impeller's C code resolves OpenGL ES entry points at context creation time.
;;; We register this CFFI callback so Impeller can query GL function pointers
;;; via SDL3.

(cffi:defcallback sdl3-gl-proc-getter :pointer
    ((proc-name :string) (user-data :pointer))
  (declare (ignore user-data))
  (%sdl3:gl-get-proc-address proc-name))

;;; ============================================================================
;;; render-stack-runtime Class
;;; ============================================================================

(defclass render-stack-runtime (render-delegate)
  ((port
    :accessor runtime-port
    :initform nil
    :documentation "Back-pointer to the owning render-stack-port.
Set during initialize-runtime.  Read-only thereafter.")
   (engine
    :accessor runtime-engine
    :initform nil
    :documentation "The render-engine instance.  Created during initialize-runtime.")
   (impeller-context
    :accessor runtime-impeller-context
    :initform nil
    :documentation "Impeller GL rendering context.  Created in realize-mirror
after the first SDL3 GL context is made current.")
   (typography-context
    :accessor runtime-typography-context
    :initform nil
    :documentation "Impeller typography context for text rendering.")
   (initialized-p
    :accessor runtime-initialized-p
    :initform nil
    :documentation "T if engine and typography context have been initialized.")
   (debug-frame-count
    :accessor runtime-debug-frame-count
    :initform 0
    :documentation "Counts frames drawn. Used by *debug-frame-limit*."))
  (:documentation "Consolidated render-stack runtime state for McCLIM.

Replaces the previous global-variable architecture:
  *global-engine*            → (runtime-engine runtime)
  *global-delegate*          → runtime (itself)
  *global-impeller-context*  → (runtime-impeller-context runtime)
  *global-engine-initialized* → (runtime-initialized-p runtime)

Window management lives on the port:
  port-window-registry  — window-id → render-stack-mirror
  port-registry-lock    — protects the registry hash table

Implements the render-delegate protocol directly — no separate delegate."))

;;; ============================================================================
;;; Initialization
;;; ============================================================================

(defgeneric initialize-runtime (runtime port)
  (:documentation "Initialize the render-engine and typography-context.

RUNTIME: The render-stack-runtime instance
PORT:    The render-stack-port (stored as runtime-port back-pointer)

Thread Contract: MUST be called on the main thread.
Called from port initialize-instance :after via rs-internals:submit-to-main-thread."))

(defmethod initialize-runtime ((runtime render-stack-runtime) port)
  "Initialize render-engine and typography-context.
MUST be called on the main thread.  Idempotent — safe to call multiple times."
  (rs-internals:assert-main-thread initialize-runtime)
  (unless (runtime-initialized-p runtime)
    (log:info :mcclim-render-stack "Initializing runtime on main thread")

    ;; Store port back-pointer before any other work.
    (setf (runtime-port runtime) port)

    ;; Initialize SDL3 video subsystem.
    (rs-sdl3:init-sdl3-video)

    ;; Create and register the SDL3 host.
    (setf (port-host port) (make-instance 'rs-sdl3:sdl3-host))

    ;; Create typography context (can be done before a GL context exists).
    (setf (runtime-typography-context runtime)
          (frs:make-typography-context))

    ;; Create and start the render engine with this runtime as the delegate.
    (setf (runtime-engine runtime)
          (render-stack:make-render-engine :delegate runtime
                                           :pipeline-depth 2
                                           :target-fps 60))
    (render-stack:render-engine-start (runtime-engine runtime))

    (setf (runtime-initialized-p runtime) t)
    (log:info :mcclim-render-stack "Runtime initialization complete")))

(defgeneric initialize-runtime-impeller-context (runtime)
  (:documentation "Create the Impeller GL rendering context on the main thread.

RUNTIME: The render-stack-runtime instance

Thread Contract: MUST be called on the main thread after an SDL3 GL context
is current (i.e. after realize-mirror creates the first SDL3 window).
Idempotent — created only once regardless of window count."))

(defmethod initialize-runtime-impeller-context ((runtime render-stack-runtime))
  "Create Impeller GL context.  MUST be called on the main thread.  Idempotent."
  (rs-internals:assert-main-thread initialize-runtime-impeller-context)
  (unless (runtime-impeller-context runtime)
    (log:info :mcclim-render-stack "Creating Impeller GL context")
    (setf (runtime-impeller-context runtime)
          (rs-internals:without-float-traps
            (frs:make-context :gl-proc-address-callback
                              (cffi:callback sdl3-gl-proc-getter))))
    (log:info :mcclim-render-stack "Impeller GL context created: ~A"
              (runtime-impeller-context runtime))))

;;; ============================================================================
;;; Render-Delegate Protocol
;;; ============================================================================
;;;
;;; The runtime IS the render-delegate.  It implements all four protocol methods.

(defmethod render-stack:render-delegate-begin-frame
    ((runtime render-stack-runtime) target-time frame-number)
  "Called on UI thread at frame start.

Phase 1: Always returns non-nil (unconditional redraw every frame).
Phase 2: Will inspect the port's dirty-sheet state.

Thread Contract: MUST be called on UI thread."
  (rs-internals:assert-ui-thread render-delegate-begin-frame)
  (format *error-output* "~&[DIAG] render-delegate-begin-frame: frame ~A~%"
          frame-number)
  ;; Phase 1: always produce a frame.
  t)

(defmethod render-stack:render-delegate-end-frame
    ((runtime render-stack-runtime) layer-tree frame-timings)
  "Called on UI thread after frame building.
Thread Contract: MUST be called on UI thread."
  (declare (ignore layer-tree frame-timings))
  (rs-internals:assert-ui-thread render-delegate-end-frame)
  nil)

(defmethod render-stack:render-delegate-notify-idle
    ((runtime render-stack-runtime) deadline)
  "Called on UI thread when engine is idle.
Thread Contract: MUST be called on UI thread."
  (declare (ignore deadline))
  (rs-internals:assert-ui-thread render-delegate-notify-idle)
  nil)

(defmethod render-stack:render-delegate-draw
    ((runtime render-stack-runtime) pipeline-item)
  "Called on main thread to rasterize.  Renders each registered mirror.

Thread Contract: MUST be called on main thread."
  (declare (ignore pipeline-item))
  (rs-internals:assert-main-thread render-delegate-draw)
  (let ((port (runtime-port runtime)))
    (unless port (return-from render-delegate-draw nil))
    ;; Debug frame limit — hard exit after N frames so we don't need kill -9.
    ;; sb-ext:exit :abort t is a POSIX _exit() that kills all threads immediately.
    ;; This is intentionally nuclear: clean shutdown requires McCLIM to cooperate
    ;; (frame-exit → event loop → runner-stop), which isn't wired up yet.
    (when *debug-frame-limit*
      (let ((n (incf (runtime-debug-frame-count runtime))))
        (when (>= n *debug-frame-limit*)
          (format *error-output* "~&[DEBUG] Frame limit ~A reached — hard exit.~%"
                  *debug-frame-limit*)
          (sb-ext:exit :code 0 :abort t))))
    (let ((mirrors (collect-registered-mirrors port)))
      (format *error-output* "~&[DIAG] render-delegate-draw: window-count=~A~%"
              (length mirrors))
      (dolist (mirror mirrors)
        (draw-test-pattern-for-mirror mirror))
      (dolist (mirror mirrors)
        (rs-sdl3:sdl3-gl-swap-window (mirror-sdl-window mirror))))))

;;; ============================================================================
;;; Direct Rendering (Phase 1)
;;; ============================================================================

(defun collect-registered-mirrors (port)
  "Return a snapshot list of all mirrors currently in PORT's registry.
Takes a lock snapshot to avoid holding the lock during rendering."
  (bt2:with-lock-held ((port-registry-lock port))
    (let ((mirrors nil))
      (maphash (lambda (id mirror)
                 (declare (ignore id))
                 (push mirror mirrors))
               (port-window-registry port))
      mirrors)))

(defun draw-test-pattern-for-mirror (mirror)
  "Draw a test pattern (white background + blue rectangle) on MIRROR.
Uses the mirror's cached FBO surface (created on first call via
get-or-create-mirror-surface).

Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread draw-test-pattern-for-mirror)
  (let ((surface (get-or-create-mirror-surface mirror)))
    (when surface
      (let ((width  (float (mirror-width  mirror) 1.0f0))
            (height (float (mirror-height mirror) 1.0f0)))
        (frs:with-display-list-builder (builder)
          ;; White background
          (let ((paint (frs:make-paint)))
            (frs:paint-set-color paint 1.0 1.0 1.0 1.0)
            (frs:draw-rect builder 0.0 0.0 width height paint)
            (frs:release-paint paint))
          ;; Blue test rectangle
          (let ((paint (frs:make-paint)))
            (frs:paint-set-color paint 0.2 0.4 0.9 1.0)
            (frs:draw-rect builder 50.0 50.0 200.0 150.0 paint)
            (frs:release-paint paint))
          ;; Execute on surface
          (rs-internals:without-float-traps
            (frs:execute-display-list surface builder)))))))

;;; ============================================================================
;;; Cleanup
;;; ============================================================================

(defun shutdown-runtime (runtime)
  "Shutdown the runtime engine and release GPU resources.
Should be called on application exit, on the main thread."
  (when (runtime-engine runtime)
    (log:info :mcclim-render-stack "Shutting down render engine")
    (render-stack:render-engine-stop (runtime-engine runtime))
    (setf (runtime-engine runtime) nil))

  (when (runtime-impeller-context runtime)
    (log:info :mcclim-render-stack "Releasing Impeller context")
    (rs-internals:without-float-traps
      (frs:release-context (runtime-impeller-context runtime)))
    (setf (runtime-impeller-context runtime) nil))

  (when (runtime-typography-context runtime)
    (log:info :mcclim-render-stack "Releasing typography context")
    (frs:release-typography-context (runtime-typography-context runtime))
    (setf (runtime-typography-context runtime) nil))

  (setf (runtime-initialized-p runtime) nil
        (runtime-port runtime) nil)
  (log:info :mcclim-render-stack "Runtime shutdown complete"))
