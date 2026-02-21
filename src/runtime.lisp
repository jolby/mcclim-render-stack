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

(defvar *debug-frame-limit* nil
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
    :documentation "Counts frames drawn. Used by *debug-frame-limit*.")
   (startup-timings
    :accessor runtime-startup-timings
    :initform nil
    :documentation "Plist of phase → ms, populated during startup for post-init inspection.
E.g. (:sdl3-video-init 8 :typography-context 45 :impeller-context 180).
Populated by initialize-runtime and initialize-runtime-impeller-context."))
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
;;; Startup invariant checking
;;; ============================================================================

(defmacro check-startup-invariant (test category message &rest args)
  "Assert TEST is true at startup. On failure, log :error and exit with code 1.
Use for preconditions that indicate an unrecoverable configuration error."
  `(unless ,test
     (log:error ,category ,message ,@args)
     (sb-ext:exit :code 1 :abort t)))

;;; ============================================================================
;;; Internal helpers
;;; ============================================================================

(defmacro %with-startup-phase (runtime phase &body body)
  "Time BODY, emit an :startup log message, and record ms to RUNTIME's startup-timings.
PHASE is an unevaluated keyword/symbol naming the startup phase.
For use within runtime initialization methods only."
  (let ((start (gensym "START-"))
        (ms    (gensym "MS-")))
    `(let ((,start (get-internal-real-time)))
       (prog1 (progn ,@body)
         (let ((,ms (round (* 1000 (- (get-internal-real-time) ,start))
                           internal-time-units-per-second)))
           (rs-internals:with-context-fields (:phase ',phase :ms ,ms)
             (log:info :startup "~A: ~Dms" ',phase ,ms))
           (setf (getf (runtime-startup-timings ,runtime) ',phase) ,ms))))))

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

    ;; Phase 0: native library must be loadable before SDL3 init.
    ;; (sdl3-video-init will load it; check here gives a clear error if missing.)
    ;; Phase 1: Initialize SDL3 video subsystem.
    (%with-startup-phase runtime :sdl3-video-init
      (rs-sdl3:init-sdl3-video))
    (check-startup-invariant (rs-sdl3:sdl3-native-lib-loaded-p)
      :startup "Fatal: SDL3 native library failed to load")
    (check-startup-invariant (rs-sdl3:sdl3-initialized-p)
      :startup "Fatal: SDL3 video subsystem failed to initialize")
    (check-startup-invariant (not (zerop (%sdl3:get-primary-display)))
      :startup "Fatal: No primary display found (SDL error: ~A)" (rs-sdl3:sdl3-get-error))

    ;; Create and register the SDL3 host.
    (setf (port-host port) (make-instance 'rs-sdl3:sdl3-host))

    ;; Phase 4: Create typography context (can be done before a GL context exists).
    (%with-startup-phase runtime :typography-context
      (setf (runtime-typography-context runtime)
            (frs:make-typography-context)))
    (check-startup-invariant (runtime-typography-context runtime)
      :startup "Fatal: Typography context creation failed")

    ;; Create and start the render engine with this runtime as the delegate.
    (%with-startup-phase runtime :render-engine-init
      (setf (runtime-engine runtime)
            (render-stack:make-render-engine :delegate runtime
                                             :pipeline-depth 2
                                             :target-fps 60))
      (render-stack:render-engine-start (runtime-engine runtime)))

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
    (%with-startup-phase runtime :impeller-context
      (setf (runtime-impeller-context runtime)
            (rs-internals:without-float-traps
              (frs:make-context :gl-proc-address-callback
                                (cffi:callback sdl3-gl-proc-getter)))))
    (check-startup-invariant (runtime-impeller-context runtime)
      :startup "Fatal: Impeller GL context creation failed (GL context may be invalid)")
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

For each mirror: if a pending display list is available (published by
medium-finish-output on the UI thread), consume it and draw it to the
mirror's FBO surface.  Falls back to the test pattern if no DL is pending.

Thread Contract: MUST be called on main thread."
  (declare (ignore pipeline-item))
  (rs-internals:assert-main-thread render-delegate-draw)
  (let ((port (runtime-port runtime)))
    (unless port (return-from render-delegate-draw nil))
    (when *debug-frame-limit*
      (let ((n (incf (runtime-debug-frame-count runtime))))
        (when (>= n *debug-frame-limit*)
          (log:info :debug "Frame limit ~A reached — hard exit." *debug-frame-limit*)
          (sb-ext:exit :code 0 :abort t))))
    (let ((mirrors (collect-registered-mirrors port)))
      (dolist (mirror mirrors)
        (let ((dl (mirror-take-pending-dl mirror)))
          (cond
            ;; First-frame path: window is hidden. Run staged reveal sequence.
            ((not (mirror-first-frame-drawn-p mirror))
             (perform-first-frame-reveal mirror dl))
            ;; McCLIM produced a display list — draw it and swap.
            (dl
             (handler-case
                 (let ((surface (get-or-create-mirror-surface mirror)))
                   (when surface
                     (rs-internals:without-float-traps
                       (frs:surface-draw-display-list surface dl))))
               (error (e)
                 (log:error :render "Error drawing display list: ~A" e)))
             (frs:release-display-list dl)
             (rs-sdl3:sdl3-gl-swap-window (mirror-sdl-window mirror)))
            ;; No DL from McCLIM — draw test pattern and swap.
            (t
             (draw-test-pattern-for-mirror mirror)
             (rs-sdl3:sdl3-gl-swap-window (mirror-sdl-window mirror)))))))))

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

(defun perform-first-frame-reveal (mirror dl)
  "Execute the staged first-frame reveal sequence for MIRROR.

DL is a pending display list (or NIL — test pattern used if NIL).
Window is hidden at call time.

Sequence:
  1. Draw content with initial (logical) dims — window still hidden.
  2. SDL_GL_SwapWindow — content committed to framebuffer.
  3. SDL_ShowWindow   — window appears already painted, no blank flash.
  4. Center window    — physical dims reliable after compositor maps window.
  5. invalidate-mirror-surface — refreshes mirror-width/height from SDL3.
     Next frame creates FBO at correct physical dims (handles HiDPI).
  6. Mark mirror-first-frame-drawn-p T.

Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread perform-first-frame-reveal)
  ;; Phase 6: surface must be creatable before we attempt to draw.
  (check-startup-invariant (get-or-create-mirror-surface mirror)
    :startup "Fatal: First frame surface creation failed for win-id=~A"
    (mirror-window-id mirror))
  (let ((runtime (port-runtime (mirror-port mirror))))
    ;; 1-3. Draw, swap, show — timed together as the first-frame reveal cost.
    (%with-startup-phase runtime :first-frame-reveal
      ;; 1. Draw content (window hidden — user doesn't see this pass).
      (if dl
          (progn
            (handler-case
                (let ((surface (get-or-create-mirror-surface mirror)))
                  (when surface
                    (rs-internals:without-float-traps
                      (frs:surface-draw-display-list surface dl))))
              (error (e)
                (log:error :render "First-frame: DL draw error: ~A" e)))
            (frs:release-display-list dl))
          (draw-test-pattern-for-mirror mirror))
      ;; 2. Commit content to framebuffer (window still hidden).
      (rs-sdl3:sdl3-gl-swap-window (mirror-sdl-window mirror))
      ;; 3. Show — window appears with content already rendered; no blank flash.
      (rs-sdl3:show-sdl3-window (mirror-sdl-window mirror))))
  ;; 4. Center — compositor has mapped the window; physical dims now reliable.
  (rs-sdl3:center-sdl3-window (mirror-sdl-window mirror))
  ;; 5. Refresh physical pixel dims. Next frame recreates FBO at correct size.
  (invalidate-mirror-surface mirror)
  ;; 6. Mark revealed — normal render path takes over.
  (setf (mirror-first-frame-drawn-p mirror) t)
  (log:info :mcclim-render-stack "First frame revealed: win-id=~A logical=~Ax~A physical=~Ax~A"
            (mirror-window-id mirror)
            (mirror-logical-width mirror) (mirror-logical-height mirror)
            (mirror-width mirror) (mirror-height mirror)))

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
