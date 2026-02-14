;;;; render-delegate.lisp — McCLIM Port as render-stack Delegate
;;;;
;;;; This is the key integration piece: the McCLIM port implements
;;;; render-stack's render-delegate protocol, allowing the render engine
;;;; to drive McCLIM's frame loop.

(in-package :mcclim-render-stack)

(defclass clim-render-delegate (render-delegate)
  ((port :initarg :port :reader delegate-port)
   (window :initarg :window :reader delegate-window)
   (impeller-context :accessor delegate-impeller-context)
   (impeller-surface :accessor delegate-impeller-surface)
   (current-builder :accessor delegate-current-builder :initform nil
                    :documentation "Current display list builder for medium drawing.")
   (frame-count :accessor delegate-frame-count :initform 0))
  (:documentation "Render delegate that draws McCLIM content via Impeller."))

(defmethod initialize-instance :after ((delegate clim-render-delegate) &key)
  "Initialize Impeller context after delegate creation."
  ;; Create Impeller context with GL proc getter
  (setf (delegate-impeller-context delegate)
        (frs:make-context :gl-proc-address-callback 
                          (cffi:callback %sdl3:gl-get-proc-address)))
  ;; Create surface for current window size
  (update-delegate-surface delegate))

(defun update-delegate-surface (delegate)
  "Create or recreate the Impeller surface for the delegate's window."
  (when (delegate-impeller-surface delegate)
    (frs:release-surface (delegate-impeller-surface delegate)))
  (let* ((window (delegate-window delegate))
         (width (rs-host:framebuffer-width window))
         (height (rs-host:framebuffer-height window)))
    (setf (delegate-impeller-surface delegate)
          (frs:make-wrapped-fbo-surface (delegate-impeller-context delegate)
                                        0 ; FBO 0 = default framebuffer
                                        width height))))

;;; Render delegate protocol implementation

(defmethod render-delegate-begin-frame ((delegate clim-render-delegate) target-time frame-number)
  "Called at start of frame - return layer tree (nil for immediate mode)."
  (declare (ignore target-time))
  (incf (delegate-frame-count delegate))
  ;; McCLIM uses immediate mode drawing, not retained layer trees
  nil)

(defmethod render-delegate-end-frame ((delegate clim-render-delegate) layer-tree frame-timings)
  "Called at end of frame."
  (declare (ignore layer-tree frame-timings)))

(defmethod render-delegate-notify-idle ((delegate clim-render-delegate) deadline)
  "Called when system is idle."
  (declare (ignore deadline)))

(defmethod render-delegate-draw ((delegate clim-render-delegate) pipeline-item)
  "Main drawing method - called by render engine each frame."
  (declare (ignore pipeline-item))
  (let ((surface (delegate-impeller-surface delegate))
        (port (delegate-port delegate)))
    (when surface
      ;; Clear with background color
      (frs:with-display-list-builder (builder)
        ;; Store builder for medium drawing operations
        (setf (delegate-current-builder delegate) builder)
        
        (let ((bg (frs:make-paint)))
          (frs:paint-set-color bg 1.0 1.0 1.0 1.0)  ; White background
          (frs:draw-rect builder 0.0 0.0 
                          (float (rs-host:framebuffer-width (delegate-window delegate)) 1.0f0)
                          (float (rs-host:framebuffer-height (delegate-window delegate)) 1.0f0)
                          bg)
          (frs:release-paint bg))
        
        ;; TODO: Iterate over port's windows and draw their content
        ;; For now, just draw a test pattern
        (draw-test-pattern builder delegate)
        
        ;; Clear the builder reference before executing
        (setf (delegate-current-builder delegate) nil)
        
        (frs:execute-display-list surface builder))
      
      ;; Swap buffers
      (%sdl3:gl-swap-window (rs-sdl3::sdl3-window-handle (delegate-window delegate))))))

(defun get-delegate-current-builder (delegate)
  "Get the current display list builder for drawing, or nil if not in a frame."
  (when delegate
    (delegate-current-builder delegate)))

(defun draw-test-pattern (builder delegate)
  "Draw a simple test pattern to verify rendering works."
  (let ((paint (frs:make-paint)))
    (unwind-protect
         (progn
           ;; Draw a blue rectangle
           (frs:paint-set-color paint 0.2 0.4 0.9 1.0)
           (frs:draw-rect builder 50.0 50.0 200.0 150.0 paint)
           
           ;; Draw a red circle
           (frs:paint-set-color paint 0.9 0.2 0.2 1.0)
           (frs:draw-oval builder 300.0 50.0 100.0 100.0 paint)
           
           ;; Draw some text
           (frs:paint-set-color paint 0.0 0.0 0.0 1.0)
           ;; Text drawing would require typography context - skip for now
           )
      (frs:release-paint paint))))

;;; Cleanup

(defmethod destroy-delegate ((delegate clim-render-delegate))
  "Clean up Impeller resources."
  (when (delegate-impeller-surface delegate)
    (frs:release-surface (delegate-impeller-surface delegate))
    (setf (delegate-impeller-surface delegate) nil))
  (when (delegate-impeller-context delegate)
    (frs:release-context (delegate-impeller-context delegate))
    (setf (delegate-impeller-context delegate) nil)))
