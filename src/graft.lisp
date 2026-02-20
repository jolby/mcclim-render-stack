(in-package :mcclim-render-stack)

;;; Graft - represents the display/screen

(defclass render-stack-graft (graft)
  ((displays :accessor graft-displays :initform nil
             :documentation "List of available displays from SDL3."))
  (:documentation "McCLIM graft representing an SDL3 display."))

(defmethod graft-width ((graft render-stack-graft) &key (units :device))
  "Return the width of the graft using render-stack-host display protocol."
  (let* ((port (port graft))
         (host (when port (port-host port)))
         (display (when host (rs-host:primary-display host)))
         (width (if display
                    (rs-host:display-width display)
                    1920)))
    (ecase units
      (:device width)
      (:millimeters (/ width (or (and display (rs-host:display-dpi display)) 96) 25.4))
      (:inches (/ width (or (and display (rs-host:display-dpi display)) 96)))
      (:screen-sized 1))))

(defmethod graft-height ((graft render-stack-graft) &key (units :device))
  "Return the height of the graft using render-stack-host display protocol."
  (let* ((port (port graft))
         (host (when port (port-host port)))
         (display (when host (rs-host:primary-display host)))
         (height (if display
                     (rs-host:display-height display)
                     1080)))
    (ecase units
      (:device height)
      (:millimeters (/ height (or (and display (rs-host:display-dpi display)) 96) 25.4))
      (:inches (/ height (or (and display (rs-host:display-dpi display)) 96)))
      (:screen-sized 1))))

(defmethod graft-orientation ((graft render-stack-graft))
  ;; Return the orientation of the display
  :default)

;;; render-stack-mirror is defined in mirror.lisp (full class with all slots).

(defmethod climi::enable-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  "Enable the sheet's mirror (make it visible)."
  ;; Currently stubbed - window is already visible after creation
  nil)

(defmethod climi::set-mirror-geometry ((port render-stack-port) sheet region)
  "Set the position and size of SHEET's mirror.
   Returns the actual geometry as multiple values (min-x min-y max-x max-y)."
  (let ((mirror (climi::sheet-direct-mirror sheet)))
    (declare (ignore mirror))
    (clim:with-bounding-rectangle* (x1 y1 x2 y2) region
      ;; Note: Window geometry updates would go here
      ;; Currently stubbed - full implementation requires SDL3 window ops
      (values x1 y1 x2 y2))))
