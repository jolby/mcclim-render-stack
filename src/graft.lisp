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

;;; Mirror protocol for grafts

(defclass render-stack-mirror ()
  ((sdl-window :accessor mirror-sdl-window :initarg :sdl-window :initform nil
               :documentation "The SDL3 window handle.")
   (impeller-context :accessor mirror-impeller-context :initform nil
                     :documentation "The Impeller rendering context."))
  (:documentation "Mirror representing an SDL3 window with Impeller context."))

(defmethod realize-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  "Create an SDL3 window for a top-level sheet.
   On the first call, also creates the render delegate and engine.
   Returns a render-stack-mirror containing the window handle."
  (let ((window (port-window port)))
    (if window
        ;; Reuse existing window for subsequent sheets (single-window mode)
        (progn
          (setf (gethash window (port-window-table port)) sheet)
          (make-instance 'render-stack-mirror :sdl-window window))
        ;; First sheet: create window, delegate, engine
        (clim:with-bounding-rectangle* (x y :width w :height h) sheet
          (let* ((title (climi::sheet-pretty-name sheet))
                 (host (port-host port))
                 (new-window (rs-host:make-window host
                                                  :title title
                                                  :width (floor w)
                                                  :height (floor h)
                                                  :x (floor x)
                                                  :y (floor y))))
            ;; Store window on port
            (setf (port-window port) new-window
                  (port-gl-context port) (rs-host:window-graphics-context new-window))
            ;; Register window → CLIM sheet mapping
            (setf (gethash new-window (port-window-table port)) sheet)
            ;; Create render delegate and engine
            (let ((delegate (make-instance 'clim-render-delegate
                                           :port port
                                           :window new-window)))
              (setf (port-delegate port) delegate)
              (let ((engine (make-render-engine :delegate delegate
                                                :pipeline-depth 2
                                                :target-fps 60)))
                (setf (port-engine port) engine)
                (render-engine-start engine)))
            ;; Start event thread now that we have a window
            (unless (port-event-thread port)
              (start-event-thread port))
            (make-instance 'render-stack-mirror :sdl-window new-window))))))

(defmethod destroy-mirror ((port render-stack-port) (sheet mirrored-sheet-mixin))
  "Destroy the mirror associated with SHEET."
  (let ((mirror (climi::sheet-direct-mirror sheet)))
    (when mirror
      (let ((window (mirror-sdl-window mirror)))
        (when window
          ;; Remove from window→sheet table
          (remhash window (port-window-table port))
          (rs-host:destroy-window (port-host port) window)))
      ;; Clear the mirror from the sheet
      (setf (climi::sheet-direct-mirror sheet) nil))))

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
