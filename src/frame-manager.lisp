;;;; frame-manager.lisp — Frame Manager for McCLIM on render-stack
;;;;
;;;; The frame manager handles adoption/disowning of application frames.
;;;; It works with the render-stack port to manage frame lifecycles.

(in-package :mcclim-render-stack)

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

;;; Initialization

(defun initialize-render-stack ()
  "Initialize the render-stack backend.
   Call this before using the :render-stack server path."
  (log:info "render-stack backend initialized"))
