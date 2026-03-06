;;;; ppm.lisp -- Framebuffer pixel snapshot: glReadPixels + PPM writer
;;;;
;;;; Tier 1 of the graphical test rig.  All GL work runs on the main thread
;;;; via *frame-capture-hook* installed at load time.
;;;;
;;;; REPL workflow:
;;;;   (setf rs-test-rig:*snapshot-path* "/tmp/frame.ppm")
;;;;   ;; next rendered frame is captured; *snapshot-path* is cleared after
;;;;
;;;; Main-thread constraint: glReadPixels reads from the currently bound GL
;;;; context.  %maybe-capture-framebuffer is called from render-delegate-draw
;;;; on the main thread, between surface-draw-display-list and GL swap.

(in-package :mcclim-render-stack/test-rig)

;;; ============================================================================
;;; GL constants
;;; ============================================================================

(defconstant +gl-rgba+          #x1908)
(defconstant +gl-unsigned-byte+ #x1401)

;;; ============================================================================
;;; Internal state
;;; ============================================================================

(defvar *snapshot-frame-counter* 0
  "Frame counter used by *snapshot-every-n-frames* for auto-numbered output paths.")

(defvar *gl-read-pixels-ptr* nil
  "Cached pointer to glReadPixels, resolved once via SDL_GL_GetProcAddress.
Must be resolved on the main thread while a GL context is current.")

;;; ============================================================================
;;; PPM writer
;;; ============================================================================

(defun write-ppm-file (path width height rgba-buffer)
  "Write RGBA-BUFFER (length width*height*4, element-type (unsigned-byte 8)) to
PATH as a P6 (binary) PPM file.  Converts RGBA->RGB and flips rows because
OpenGL glReadPixels returns rows bottom-up while PPM is top-down.
Returns PATH on success."
  (with-open-file (stream path
                   :direction :output
                   :element-type '(unsigned-byte 8)
                   :if-exists :supersede)
    ;; ASCII header: "P6\n<width> <height>\n255\n"
    (let ((header (format nil "P6~%~D ~D~%255~%" width height)))
      (loop for c across header do
        (write-byte (char-code c) stream)))
    ;; Pixel data: top row first (flip from GL's bottom-up order)
    (loop for row from (1- height) downto 0 do
      (let ((row-offset (* row width 4)))
        (loop for col from 0 below width do
          (let ((i (+ row-offset (* col 4))))
            (write-byte (aref rgba-buffer i)       stream)  ; R
            (write-byte (aref rgba-buffer (+ i 1)) stream)  ; G
            (write-byte (aref rgba-buffer (+ i 2)) stream))))))  ; B, skip A
  path)

;;; ============================================================================
;;; Framebuffer capture
;;; ============================================================================

(defun %maybe-capture-framebuffer (mirror)
  "Called on the main thread after surface-draw-display-list, before GL swap.
Reads the current back buffer to a PPM file when *snapshot-path* is set or
*snapshot-every-n-frames* triggers.  Thread Contract: MUST be called on main
thread while the mirror's GL context is current."
  (let ((w (mcclim-render-stack:mirror-width mirror))
        (h (mcclim-render-stack:mirror-height mirror)))
    (when (and (plusp w) (plusp h))
      (let ((path
              (cond
                ;; One-shot: clear after capture so only one frame is saved.
                (*snapshot-path*
                 (prog1 *snapshot-path*
                   (setf *snapshot-path* nil)))
                ;; Periodic: auto-number files under /tmp/.
                (*snapshot-every-n-frames*
                 (let ((n (incf *snapshot-frame-counter*)))
                   (when (zerop (mod n *snapshot-every-n-frames*))
                     (format nil "/tmp/frame-~6,'0D.ppm" n)))))))
        (when path
          (handler-case
              (let ((gl-read-pixels
                      ;; glReadPixels is not in a statically-linked lib on most
                      ;; platforms -- resolve via SDL's proc address getter,
                      ;; same mechanism Impeller uses.  Cache after first call.
                      (or *gl-read-pixels-ptr*
                          (let ((ptr (sdl3-ffi:gl-get-proc-address "glReadPixels")))
                            (when (cffi:null-pointer-p ptr)
                              (error "SDL_GL_GetProcAddress(\"glReadPixels\") returned NULL"))
                            (setf *gl-read-pixels-ptr* ptr)))))
                (cffi:with-foreign-object (buf :unsigned-char (* w h 4))
                  (cffi:foreign-funcall-pointer gl-read-pixels ()
                    :int 0 :int 0 :int w :int h
                    :unsigned-int +gl-rgba+
                    :unsigned-int +gl-unsigned-byte+
                    :pointer buf
                    :void)
                  (let ((rgba (make-array (* w h 4) :element-type '(unsigned-byte 8))))
                    (dotimes (i (* w h 4))
                      (setf (aref rgba i) (cffi:mem-aref buf :unsigned-char i)))
                    (write-ppm-file path w h rgba)
                    (log:info :test-rig "Captured ~Ax~A framebuffer to ~A" w h path))))
            (error (e)
              (log:error :test-rig "Framebuffer capture failed: ~A" e))))))))

;;; ============================================================================
;;; Install hook
;;; ============================================================================

;;; Install the capture function into the main system's hook slot so
;;; render-delegate-draw can call us without a hard compile-time dependency.
(setf mcclim-render-stack:*frame-capture-hook*
      (lambda (mirror) (%maybe-capture-framebuffer mirror)))
