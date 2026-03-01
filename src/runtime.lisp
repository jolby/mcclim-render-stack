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

(defvar *render-assertions-enabled* t
  "When T, render-pipeline invariants are checked on every draw cycle.
Set to NIL to disable (e.g., for benchmarking or when the debugger is unwanted).

In an interactive REPL (no --disable-debugger): a violated invariant calls
BREAK and freezes the frame in the debugger so all state can be inspected.
In batch mode (--disable-debugger): prints to stderr and continues.")

(defmacro check-render-invariant (test message &rest args)
  "Assert TEST is true at a render-pipeline checkpoint.

On failure (TEST is nil):
  1. Logs the violation at :error level (always visible regardless of log level).
  2. BREAKs into the debugger with the full violation message.

In an interactive REPL: execution suspends; inspect mirror, snapshot, surface,
etc.; continue with the CONTINUE restart to resume the render loop.

In batch mode (--disable-debugger): SBCL prints the message and continues
automatically, so the render loop keeps running for diagnosis.

Guarded by *render-assertions-enabled* -- set to NIL to silence assertions."
  `(when *render-assertions-enabled*
     (unless ,test
       (let ((msg (format nil ,message ,@args)))
         (log:error :render "Render invariant violated: ~A" msg)
         (break "Render invariant violated: ~A~%~%Tip: (setf mcclim-render-stack::*render-assertions-enabled* nil) to mute"
                msg)))))

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
Populated by initialize-runtime and initialize-runtime-impeller-context.")
   (flow-context
    :accessor runtime-flow-context
    :initform nil
    :documentation "Long-lived Flow compositor context with raster cache.
Created in initialize-runtime-impeller-context after the Impeller context is ready.
Shared across all mirrors -- not tied to a specific window.
Released in shutdown-runtime."))
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
     (uiop:quit 1)))

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
  ;; Ensure verbose global controller is running so log output reaches
  ;; stdout/stderr even in batch SBCL (make demo, CI).  Must be called
  ;; before any other log:* call in this method.
  ;; (rs-internals:ensure-logging-initialized)
  (rs-internals:setup-dev-logging)
  (log:info :initialize-runtime "Logging initialized!")
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
              (runtime-impeller-context runtime)))
  (unless (runtime-flow-context runtime)
    (%with-startup-phase runtime :flow-context
      (setf (runtime-flow-context runtime)
            (rs-internals:without-float-traps
              (frs:make-compositor-context))))
    (check-startup-invariant (runtime-flow-context runtime)
      :startup "Fatal: Flow compositor context creation failed")
    (log:info :mcclim-render-stack "Flow compositor context created")))

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

For each mirror: if frame-dirty-p is set (by medium-finish-output on the
UI thread), snapshot pane DLs, composite them into a single DL, and draw
it to the mirror FBO surface.  Falls back to the retained current-dl or
test pattern when nothing is dirty.

Thread Contract: MUST be called on main thread."
  (declare (ignore pipeline-item))
  (rs-internals:assert-main-thread render-delegate-draw)
  (let ((port (runtime-port runtime)))
    (unless port
      (log:warn :render "Nil Port passed to render-delegate-draw!. Doing nothing...")
      (return-from render-delegate-draw nil))
    (when *debug-frame-limit*
      (let ((n (incf (runtime-debug-frame-count runtime))))
        (when (>= n *debug-frame-limit*)
          (log:info :debug "Frame limit ~A reached - hard exit." *debug-frame-limit*)
          (uiop:quit 0))))
    (let ((mirrors (collect-registered-mirrors port)))
      (dolist (mirror mirrors)
        ;; Get surface early — needed by %composite-via-flow to draw inside
        ;; the scoped frame (scene DL may reference frame-scoped state).
        (let ((surface (get-or-create-mirror-surface mirror)))
          ;; Snapshot pane DLs when frame-dirty-p is set, then composite via Flow.
          (let* ((snapshot
                  (when (bt2:with-lock-held ((mirror-dl-lock mirror))
                          (mirror-frame-dirty-p mirror))
                    (mirror-snapshot-pane-dls mirror)))
                 (new-dl
                  (when snapshot
                    (%composite-via-flow runtime mirror snapshot surface))))
            (log:info :render "render-delegate-draw: first=~A dirty=~A snap=~A newdl=~A currdl=~A dims=~Ax~A"
                      (mirror-first-frame-drawn-p mirror)
                      (not (null snapshot))
                      (if snapshot (length snapshot) 0)
                      (not (null new-dl))
                      (not (null (mirror-current-dl mirror)))
                      (mirror-width mirror) (mirror-height mirror))
            ;; Invariant: a non-empty snapshot must always yield a composite DL.
            (check-render-invariant
             (or (null snapshot) new-dl)
             "composite returned nil for non-empty snapshot (n=~A); win=~A dims=~Ax~A"
             (if snapshot (length snapshot) 0)
             (mirror-window-id mirror) (mirror-width mirror) (mirror-height mirror))
            ;; Snapshot lifecycle: scene-dl (current-dl) contains raw void*
            ;; references to pane ImpellerDisplayLists via recorded DrawDisplayList
            ;; ops. FlowDisplayListLayerNew does not retain them and neither does
            ;; scoped-frame-build-display-list. If snapshot DLs are released before
            ;; current-dl is replaced, retained draws dereference freed pointers
            ;; and produce black output.
            ;;
            ;; Correct pattern: keep snapshot alive as composite-deps while
            ;; current-dl references them. Release OLD composite-deps (for old
            ;; current-dl) when a new composite succeeds. If composite failed
            ;; (new-dl nil), release snapshot immediately (no new current-dl).
            (cond
              ((and snapshot new-dl)
               ;; Composite succeeded: swap old deps for new snapshot.
               (%release-composite-deps mirror)
               (setf (mirror-composite-deps mirror) snapshot))
              (snapshot
               ;; Composite failed: release snapshot now; old deps stay live.
               (dolist (entry snapshot)
                 (frs:release-display-list (cdr entry)))))
            (cond
              ;; First-frame reveal: window is still hidden. Run staged sequence.
              ((not (mirror-first-frame-drawn-p mirror))
               (perform-first-frame-reveal mirror new-dl))
              ;; New composite DL — already drawn to surface by %composite-via-flow.
              ;; Store for retained redraw, then swap.
              (new-dl
               (let ((old (mirror-current-dl mirror)))
                 (when old (frs:release-display-list old)))
               (setf (mirror-current-dl mirror) new-dl)
               (log:info :render "composite draw: surface=~A dims=~Ax~A"
                         (not (null surface)) (mirror-width mirror) (mirror-height mirror))
               (rs-sdl3:sdl3-gl-swap-window (mirror-sdl-window mirror)))
              ;; Not dirty — re-composite from stored pane DLs.
              ;; DL reuse (drawing mirror-current-dl directly) produces black —
              ;; we do not know why yet (add draw-result logging above to
              ;; investigate). Rebuilding the layer tree each retained frame
              ;; matches the working impeller-flow-composition demo pattern
              ;; exactly and avoids whatever failure mode afflicts DL reuse.
              (t
               (let ((deps (mirror-composite-deps mirror)))
                 (if (and deps surface)
                     (%retained-redraw runtime mirror deps surface)
                     (draw-test-pattern-for-mirror mirror)))
               (rs-sdl3:sdl3-gl-swap-window (mirror-sdl-window mirror))))))))))

;;; ============================================================================
;;; Direct Rendering
;;; ============================================================================

(defun %release-composite-deps (mirror)
  "Release all snapshot DLs stored as composite dependencies on MIRROR.
These are the sub-DLs referenced by mirror-current-dl's composite.
Thread Contract: MUST be called on main thread."
  (dolist (entry (mirror-composite-deps mirror))
    (frs:release-display-list (cdr entry)))
  (setf (mirror-composite-deps mirror) nil))

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

(defun %sheet-depth (sheet)
  "Return the depth of SHEET in the sheet hierarchy (0 = root).
Used to sort pane DLs for back-to-front compositing (painter's algorithm)."
  (loop for s = (clim:sheet-parent sheet) then (clim:sheet-parent s)
        while s
        count 1))


(defun perform-first-frame-reveal (mirror dl)
  "Execute the staged first-frame reveal sequence for MIRROR.

DL is a pending display list (or NIL — test pattern used if NIL).
Window is hidden at call time.

Sequence:
  1. Draw content with initial (logical) dims — window still hidden.
  2. SDL_GL_SwapWindow — content committed to framebuffer.
  3. SDL_ShowWindow   — window appears already painted, no blank flash.
  4. Center window    — physical dims reliable after compositor maps window.
  5. Refresh dims from SDL3; only release FBO surface if dims changed (HiDPI).
     Releasing a same-size surface corrupts GL state → black frames.
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
            ;; Retain DL for subsequent frames — do NOT release here.
            (setf (mirror-current-dl mirror) dl))
          (draw-test-pattern-for-mirror mirror))
      ;; 2. Commit content to framebuffer (window still hidden).
      (rs-sdl3:sdl3-gl-swap-window (mirror-sdl-window mirror))
      ;; 3. Show — window appears with content already rendered; no blank flash.
      (rs-sdl3:show-sdl3-window (mirror-sdl-window mirror))))
  ;; 4. Center — compositor has mapped the window; physical dims now reliable.
  (rs-sdl3:center-sdl3-window (mirror-sdl-window mirror))
  ;; 5. Refresh physical pixel dims from SDL3.
  ;;    Only release the FBO surface if dims changed (e.g. HiDPI 2x upscale).
  ;;    Releasing an Impeller surface that wraps FBO 0 appears to corrupt
  ;;    GL state — subsequent surface-draw-display-list calls produce black.
  ;;    All working demos keep the surface object alive across frames; we
  ;;    match that pattern by preserving the surface when dims are unchanged.
  (let* ((win    (mirror-sdl-window mirror))
         (new-w  (when win (rs-host:framebuffer-width  win)))
         (new-h  (when win (rs-host:framebuffer-height win))))
    (if (and new-w new-h (plusp new-w) (plusp new-h))
        (let ((dims-changed (or (/= new-w (mirror-width  mirror))
                                (/= new-h (mirror-height mirror)))))
          (when dims-changed
            ;; Dims changed (HiDPI) — must recreate surface at new pixel size.
            (when (mirror-surface mirror)
              (rs-internals:without-float-traps
                (frs:release-surface (mirror-surface mirror)))
              (setf (mirror-surface mirror) nil)))
          (setf (mirror-width  mirror) new-w
                (mirror-height mirror) new-h)
          (log:info :render "perform-first-frame-reveal: phys=~Ax~A surface=~A"
                    new-w new-h (if dims-changed "released(HiDPI)" "kept")))
        (log:warn :render "perform-first-frame-reveal: SDL3 zero dims (~Ax~A), retaining ~Ax~A"
                  (or new-w 0) (or new-h 0) (mirror-width mirror) (mirror-height mirror))))
  ;; 6. Mark revealed — normal render path takes over.
  (setf (mirror-first-frame-drawn-p mirror) t)
  (log:info :mcclim-render-stack "First frame revealed: win-id=~A logical=~Ax~A physical=~Ax~A"
            (mirror-window-id mirror)
            (mirror-logical-width mirror) (mirror-logical-height mirror)
            (mirror-width mirror) (mirror-height mirror)))

(defun %retained-redraw (runtime mirror deps surface)
  "Re-composite from DEPS (stored pane DLs) without updating persistent state.
DEPS is an alist ((sheet . dl) ...) from mirror-composite-deps.
Builds a fresh layer tree, rasterizes, draws to SURFACE, then releases.
Does NOT update mirror-composite-deps or mirror-previous-layer-tree.
Thread Contract: MUST be called on the main thread."
  (rs-internals:assert-main-thread %retained-redraw)
  (let* ((flow-ctx (runtime-flow-context runtime))
         (phys-w   (mirror-width  mirror))
         (phys-h   (mirror-height mirror)))
    (unless (and flow-ctx (plusp phys-w) (plusp phys-h))
      (log:warn :render "%retained-redraw: missing flow-ctx or zero dims")
      (return-from %retained-redraw nil))
    (let ((tree (build-pane-layer-tree mirror deps)))
      (unless tree
        (log:warn :render "%retained-redraw: build-pane-layer-tree returned nil")
        (return-from %retained-redraw nil))
      (handler-case
          (rs-internals:without-float-traps
            (let (frame-dl)
              (frs:with-scoped-frame (frame flow-ctx (cffi:null-pointer) (cons phys-w phys-h))
                (when (eq (frs:scoped-frame-raster frame tree :ignore-raster-cache t) :success)
                  (setf frame-dl (frs:scoped-frame-build-display-list frame))))
              ;; GL state reset — same rationale as %composite-via-flow.
              (when (and frame-dl surface)
                (let ((bind-fb (%sdl3:gl-get-proc-address "glBindFramebuffer")))
                  (unless (cffi:null-pointer-p bind-fb)
                    (cffi:foreign-funcall-pointer bind-fb () :unsigned-int #x8D40 :unsigned-int 0 :void)))
                (let ((draw-ok (frs:surface-draw-display-list surface frame-dl)))
                  (log:info :render "%retained-redraw: draw-result=~A" draw-ok))
                (frs:release-display-list frame-dl))))
        (error (e)
          (log:error :render "%retained-redraw: ~A" e)))
      (frs:release-layer-tree tree))))

(defun %redraw-retained-tree (runtime mirror surface tree)
  "Re-composite TREE to SURFACE using a new scoped frame.

Called on retained (not-dirty) frames when a previous layer tree exists.
The scene DL from the previous composite is frame-scoped and cannot be
reused; we re-rasterize the retained layer tree from scratch each frame.

Thread Contract: MUST be called on the main thread."
  (rs-internals:assert-main-thread %redraw-retained-tree)
  (let* ((flow-ctx (runtime-flow-context runtime))
         (phys-w   (mirror-width  mirror))
         (phys-h   (mirror-height mirror)))
    (unless (and flow-ctx surface (plusp phys-w) (plusp phys-h))
      (log:warn :render "%redraw-retained-tree: missing context/surface or zero dims")
      (return-from %redraw-retained-tree nil))
    (rs-internals:without-float-traps
      (frs:with-scoped-frame (frame flow-ctx (cffi:null-pointer) (cons phys-w phys-h))
        (let ((status (frs:scoped-frame-raster frame tree :ignore-raster-cache t)))
          (log:info :render "%redraw-retained-tree: raster status=~A" status)
          (when (eq status :success)
            (let ((dl (frs:scoped-frame-build-display-list frame)))
              (when dl
                (frs:surface-draw-display-list surface dl)
                (frs:release-display-list dl)))))))))

(defun draw-test-pattern-for-mirror (mirror)
  "Draw a test pattern (white background + blue rectangle) on MIRROR.
Uses the mirror's cached FBO surface (created on first call via
get-or-create-mirror-surface).

Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread draw-test-pattern-for-mirror)
  (let ((surface (get-or-create-mirror-surface mirror)))
    (log:info :render "draw-test-pattern: surface=~A dims=~Ax~A"
              (not (null surface)) (mirror-width mirror) (mirror-height mirror))
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

  (when (runtime-flow-context runtime)
    (log:info :mcclim-render-stack "Releasing Flow compositor context")
    (rs-internals:without-float-traps
      (frs:release-compositor-context (runtime-flow-context runtime)))
    (setf (runtime-flow-context runtime) nil))

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
