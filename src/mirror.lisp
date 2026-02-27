;;;; mirror.lisp — render-stack-mirror class, registry, and surface lifecycle
;;;;
;;;; The mirror is the canonical per-window object. It holds:
;;;;   - The SDL3 window and GL context (native side)
;;;;   - The cached Impeller FBO surface (render side)
;;;;   - Back-pointers to the owning port and McCLIM sheet
;;;;
;;;; File responsibilities:
;;;;   render-stack-mirror class definition
;;;;   register-mirror / deregister-mirror / find-mirror-by-window-id / find-sheet-by-window-id
;;;;   get-or-create-mirror-surface / invalidate-mirror-surface
;;;;
;;;; NOTE: realize-mirror / destroy-mirror live in port.lisp where
;;;;   render-stack-port is defined (class specializer required at load time).

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; Mirror Class
;;; ============================================================================

(defclass render-stack-mirror ()
  ((sdl-window
    :initarg  :sdl-window
    :accessor mirror-sdl-window
    :documentation "SDL3 window object (rs-sdl3 wrapper).")
   (window-id
    :initarg  :window-id
    :accessor mirror-window-id
    :documentation "SDL3 window ID (integer). Key in port-window-registry.")
   (gl-context
    :initarg  :gl-context
    :accessor mirror-gl-context
    :documentation "SDL3 GL context for this window.")
   (port
    :initarg  :port
    :accessor mirror-port
    :documentation "Back-pointer to the owning render-stack-port.")
   (sheet
    :initarg  :sheet
    :accessor mirror-sheet
    :documentation "Back-pointer to the McCLIM sheet.")
   (surface
    :accessor mirror-surface
    :initform nil
    :documentation "Cached Impeller FBO surface. NIL until first render.
Created by get-or-create-mirror-surface, invalidated on resize, released on destroy.")
   (width
    :initarg  :width
    :accessor mirror-width
    :documentation "Framebuffer width in physical pixels (SDL_GetWindowSizeInPixels).")
   (height
    :initarg  :height
    :accessor mirror-height
    :documentation "Framebuffer height in physical pixels (SDL_GetWindowSizeInPixels).")
   (logical-width
    :initarg  :logical-width
    :accessor mirror-logical-width
    :documentation "Window width in logical (device-independent) pixels from frame geometry.")
   (logical-height
    :initarg  :logical-height
    :accessor mirror-logical-height
    :documentation "Window height in logical (device-independent) pixels from frame geometry.")
   (pane-dl-map
    :initform (make-hash-table :test 'eq)
    :accessor mirror-pane-dl-map
    :documentation "Hash table mapping McCLIM sheet -> retained Impeller display-list.
Protected by dl-lock. Each entry is the most recent DL produced by that pane's medium.
Entries are replaced (old DL released) each time a pane redraws.
Released on destroy-mirror.")
   (frame-dirty-p
    :initform nil
    :accessor mirror-frame-dirty-p
    :documentation "T when at least one pane DL has been updated since the last composite.
Set by medium-finish-output (UI thread, under dl-lock).
Cleared by render-delegate-draw after compositing (main thread, under dl-lock).")
   (pending-dl
    :initform nil
    :accessor mirror-pending-dl
    :documentation "Completed Impeller display list waiting to be rasterized by the main thread.
Protected by dl-lock. Set by mirror-store-pending-dl (UI thread),
consumed and cleared by mirror-take-pending-dl (main thread).")
   (current-dl
    :initform nil
    :accessor mirror-current-dl
    :documentation "Last successfully rendered display list, retained for redraw each frame.
Written and read exclusively on the main thread -- no lock needed.
Released on destroy-mirror or when replaced by a new DL.")
   (composite-deps
    :initform nil
    :accessor mirror-composite-deps
    :documentation "Snapshot alist ((sheet . dl) ...) whose DLs are referenced by
current-dl's composite.  Kept alive until current-dl is replaced/released.
Main-thread-owned -- no lock needed.")
   (dl-lock
    :initform (bt2:make-lock :name "mirror-dl")
    :reader mirror-dl-lock
    :documentation "Lock protecting pending-dl for cross-thread access.")
   (scale
    :initarg  :scale
    :initform 1.0f0
    :accessor mirror-scale
    :documentation "Display content scale factor (HiDPI).
   Ratio of physical pixels to logical pixels, e.g. 2.0 on Retina displays.")
   (first-frame-drawn-p
    :initform nil
    :accessor mirror-first-frame-drawn-p
    :documentation "T after the first complete frame (draw + gl-swap-buffer + show) has executed.
On nil→T transition the window is shown and mirror-width/height refreshed from SDL3.
The render-delegate-draw first-frame path sets this after perform-first-frame-reveal."))
  (:documentation
   "The canonical McCLIM mirror for an SDL3 window.

Holds all per-window state: the SDL3 window handle, GL context, cached
Impeller FBO surface, and back-pointers to the owning port and sheet.

Created by realize-mirror, destroyed by destroy-mirror.
Registered in port-window-registry by window-id for O(1) event routing."))

;;; ============================================================================
;;; Window Registry
;;; ============================================================================
;;;
;;; Thread-safe window-id → mirror mapping.  Stored on the port.
;;; registry-lock protects the hash table.

(defun register-mirror (port mirror)
  "Register MIRROR in PORT's window registry under its window-id.
Thread Contract: May be called from any thread. Acquires port-registry-lock."
  (rs-internals:with-context-fields (:win (mirror-window-id mirror))
    (log:debug :registry "Registering mirror"))
  (bt2:with-lock-held ((port-registry-lock port))
    (setf (gethash (mirror-window-id mirror)
                   (port-window-registry port))
          mirror)))

(defun deregister-mirror (port mirror)
  "Remove MIRROR from PORT's window registry.
Thread Contract: May be called from any thread. Acquires port-registry-lock."
  (bt2:with-lock-held ((port-registry-lock port))
    (remhash (mirror-window-id mirror)
             (port-window-registry port))))

(defun find-mirror-by-window-id (port window-id)
  "Return the render-stack-mirror for WINDOW-ID, or NIL if not registered.
Thread Contract: May be called from any thread. Acquires port-registry-lock."
  (bt2:with-lock-held ((port-registry-lock port))
    (gethash window-id (port-window-registry port))))

(defun find-sheet-by-window-id (port window-id)
  "Return the McCLIM sheet for WINDOW-ID, or NIL if not registered."
  (let ((mirror (find-mirror-by-window-id port window-id)))
    (when mirror
      (mirror-sheet mirror))))

;;; ============================================================================
;;; Surface Lifecycle
;;; ============================================================================
;;;
;;; Each mirror caches exactly one Impeller FBO surface wrapping the window's
;;; default framebuffer (FBO 0).  The surface is created on first use and
;;; invalidated (released) on window resize or mirror destruction.

(defun get-or-create-mirror-surface (mirror)
  "Return the cached Impeller FBO surface for MIRROR, creating it if needed.
Thread Contract: MUST be called on the main thread."
  (rs-internals:assert-main-thread get-or-create-mirror-surface)
  (or (mirror-surface mirror)
      (let* ((port    (mirror-port mirror))
             (runtime (port-runtime port))
             (ctx     (runtime-impeller-context runtime))
             (width   (mirror-width mirror))
             (height  (mirror-height mirror)))
        (when (and ctx (plusp width) (plusp height))
          (let ((surface (rs-internals:without-float-traps
                           (frs:make-wrapped-fbo-surface ctx 0 width height))))
            (setf (mirror-surface mirror) surface)
            surface)))))

(defun invalidate-mirror-surface (mirror)
  "Release MIRROR's cached FBO surface and refresh its size from SDL3.
Call this on window resize so the next frame creates a correctly-sized surface.
Thread Contract: MUST be called on the main thread."
  (rs-internals:assert-main-thread invalidate-mirror-surface)
  (when (mirror-surface mirror)
    (rs-internals:without-float-traps
      (frs:release-surface (mirror-surface mirror)))
    (setf (mirror-surface mirror) nil))
  ;; Refresh dimensions from the SDL3 window (pixel / framebuffer size).
  ;; Guard: on Wayland the compositor may not have mapped the window yet even
  ;; after SDL_ShowWindow, so SDL_GetWindowSizeInPixels can return 0.  If we
  ;; store 0 dims the next composite gets scale=0 (invisible) and
  ;; get-or-create-mirror-surface returns nil -- both cause a black frame.
  ;; Retain the previous (logical) dims when SDL3 reports zero.
  (let ((window (mirror-sdl-window mirror)))
    (when window
      (let ((new-w (rs-host:framebuffer-width  window))
            (new-h (rs-host:framebuffer-height window)))
        (if (and (plusp new-w) (plusp new-h))
            (progn
              (setf (mirror-width  mirror) new-w
                    (mirror-height mirror) new-h)
              (log:debug :render "invalidate-mirror-surface: phys dims=~Ax~A" new-w new-h))
            (log:warn :render "invalidate-mirror-surface: SDL3 zero dims (~Ax~A), retaining ~Ax~A"
                      new-w new-h (mirror-width mirror) (mirror-height mirror)))))))

;;; ============================================================================
;;; Cross-Thread Display List Handoff
;;; ============================================================================
;;;
;;; The UI thread builds a display list (via %get-medium-builder /
;;; medium-finish-output) and publishes it here.  The main thread
;;; consumes it in render-delegate-draw.

(defun mirror-take-pending-dl (mirror)
  "Atomically take (and clear) MIRROR's pending display list.
Returns the DL (or NIL), transferring ownership to the caller.
Thread Contract: May be called from any thread. Acquires dl-lock."
  (bt2:with-lock-held ((mirror-dl-lock mirror))
    (let ((dl (mirror-pending-dl mirror)))
      (setf (mirror-pending-dl mirror) nil)
      dl)))

(defun mirror-store-pending-dl (mirror new-dl)
  "Store NEW-DL as MIRROR's pending display list.
If a previous DL was never consumed, it is released immediately.
Thread Contract: May be called from any thread. Acquires dl-lock."
  (let (old-dl)
    (bt2:with-lock-held ((mirror-dl-lock mirror))
      (setf old-dl (mirror-pending-dl mirror)
            (mirror-pending-dl mirror) new-dl))
    (when old-dl
      (frs:release-display-list old-dl))))

(defun mirror-snapshot-pane-dls (mirror)
  "Return a snapshot alist ((sheet . dl) ...) with each DL retained.
Under dl-lock, shallow-copies the pane-dl-map and retains each DL so the
snapshot is safe to use after the lock is released.  Clears frame-dirty-p.

Caller MUST release each DL in the snapshot after use (e.g. with frs:release-display-list).

Thread Contract: May be called from any thread. Acquires dl-lock."
  (let (snapshot)
    (bt2:with-lock-held ((mirror-dl-lock mirror))
      (maphash (lambda (sheet dl)
                 (frs:retain-display-list dl)
                 (push (cons sheet dl) snapshot))
               (mirror-pane-dl-map mirror))
      (setf (mirror-frame-dirty-p mirror) nil))
    snapshot))

;;; realize-mirror and destroy-mirror are defined in port.lisp.
