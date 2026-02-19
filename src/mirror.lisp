;;;; mirror.lisp — render-stack-mirror class, registry, and surface lifecycle
;;;;
;;;; The mirror is the canonical per-window object. It holds:
;;;;   - The SDL3 window and GL context (native side)
;;;;   - The cached Impeller FBO surface (render side)
;;;;   - Back-pointers to the owning port and McCLIM sheet
;;;;
;;;; File responsibilities:
;;;;   render-stack-mirror class definition
;;;;   realize-mirror / destroy-mirror (McCLIM mirror protocol)
;;;;   register-mirror / deregister-mirror / find-mirror-by-window-id / find-sheet-by-window-id
;;;;   get-or-create-mirror-surface / invalidate-mirror-surface

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
    :documentation "Framebuffer width in pixels.")
   (height
    :initarg  :height
    :accessor mirror-height
    :documentation "Framebuffer height in pixels."))
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
  (let ((window (mirror-sdl-window mirror)))
    (when window
      (setf (mirror-width  mirror) (rs-host:framebuffer-width  window)
            (mirror-height mirror) (rs-host:framebuffer-height window)))))

;;; ============================================================================
;;; McCLIM Mirror Protocol
;;; ============================================================================

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
