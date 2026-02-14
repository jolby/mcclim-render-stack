(in-package :mcclim-render-stack)

;;; Graft - represents the display/screen

(defclass render-stack-graft (graft)
  ((displays :accessor graft-displays :initform nil
             :documentation "List of available displays from SDL3."))
  (:documentation "McCLIM graft representing an SDL3 display."))

(defun graft-width (graft)
  "Return the width of the graft in pixels."
  (declare (ignore graft))
  1920)

(defun graft-height (graft)
  "Return the height of the graft in pixels."
  (declare (ignore graft))
  1080)

(defmethod graft-orientation ((graft render-stack-graft))
  ;; Return the orientation of the display
  :default)

;;; Mirror protocol for grafts

(defclass render-stack-mirror ()
  ((sdl-window :accessor mirror-sdl-window :initarg :sdl-window :initform nil
               :documentation "The SDL3 window handle.")
   (impeller-context :accessor mirror-impeller-context :initform nil
                     :documentation "The Impeller rendering context."))
  (:documentation "Mirror representing an SDL3 window with Impeller context."))

(defmethod realize-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  ;; Create an SDL3 window for this sheet
  ;; Returns a render-stack-mirror
  (make-instance 'render-stack-mirror))

(defmethod destroy-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  ;; Destroy the SDL3 window
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      ;; Cleanup SDL3 window and Impeller context
      )))

(defmethod port-set-mirror-geometry ((port render-stack-port) sheet region)
  ;; Set the mirror's geometry (position and size)
  (declare (ignore sheet region)))
