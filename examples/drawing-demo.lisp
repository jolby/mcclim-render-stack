
(ql:quickload :mcclim-render-stack :silent t)
(in-package :clim-user)

(define-application-frame drawing-demo ()
  ()
  (:panes
   (canvas :application
           :display-function 'display-canvas
           :scroll-bars nil
           :background +white+))
  (:layouts
   (default
    (vertically ()
      (outlining (:thickness 2)
        (labelling (:label "Impeller Drawing Demo")
          (outlining (:thickness 10)
            canvas)))))))

(defun display-canvas (frame stream)
  (declare (ignore frame))
  (let ((x 20) (y 20))
    ;; 1. Basic Shapes
    (draw-text* stream "Basic Shapes:" x y :align-y :top)
    (incf y 30)
    
    ;; Point
    (draw-point* stream (+ x 10) (+ y 10) :ink +red+ :line-thickness 5)
    (draw-text* stream "Point" (+ x 30) (+ y 15) :align-y :center)
    
    ;; Line
    (draw-line* stream (+ x 100) y (+ x 150) (+ y 30) :ink +blue+ :line-thickness 2)
    (draw-text* stream "Line" (+ x 160) (+ y 15) :align-y :center)
    
    ;; Rectangle (Outline - not implemented yet, so filled)
    (draw-rectangle* stream (+ x 220) y (+ x 260) (+ y 30) :ink +green+ :filled t)
    (draw-text* stream "Rect (Filled)" (+ x 270) (+ y 15) :align-y :center)
    
    (incf y 50)
    
    ;; 2. Polygons and Ellipses
    (draw-text* stream "Polygons & Ellipses:" x y :align-y :top)
    (incf y 30)
    
    ;; Polygon
    (draw-polygon* stream (list (+ x 10) (+ y 30)
                                (+ x 30) y
                                (+ x 50) (+ y 30))
                   :ink +orange+ :filled t)
    (draw-text* stream "Polygon" (+ x 60) (+ y 15) :align-y :center)
    
    ;; Ellipse/Circle
    (draw-ellipse* stream (+ x 200) (+ y 15) 15 0 0 15 
                   :ink +purple+ :filled t)
    (draw-text* stream "Circle" (+ x 230) (+ y 15) :align-y :center)
    
    (incf y 50)
    
    ;; 3. Transformations
    (draw-text* stream "Transformations:" x y :align-y :top)
    (incf y 30)
    
    (with-drawing-options (stream :ink +black+)
      (draw-rectangle* stream x y (+ x 20) (+ y 20) :filled t)
      (draw-text* stream "Normal" (+ x 30) (+ y 10) :align-y :center))
    
    (with-drawing-options (stream :transformation (make-translation-transformation 150 0))
      (draw-rectangle* stream x y (+ x 20) (+ y 20) :filled t :ink +red+)
      (draw-text* stream "Translated" (+ x 30) (+ y 10) :align-y :center))
      
    (incf y 40)
    
    ;; Scaling
    (with-drawing-options (stream :transformation (make-scaling-transformation 1.5 1.5))
      (draw-text* stream "Scaled Text (1.5x)" (/ x 1.5) (/ y 1.5) :align-y :top))
      
    (incf y 40)
    
    ;; 4. Text Alignment
    (draw-text* stream "Text Alignment:" x y :align-y :top)
    (incf y 30)
    
    (let ((tx 100))
      (draw-line* stream tx y tx (+ y 60) :ink +gray+)
      (draw-text* stream "Left" tx (+ y 10) :align-x :left :ink +black+)
      (draw-text* stream "Center" tx (+ y 30) :align-x :center :ink +blue+)
      (draw-text* stream "Right" tx (+ y 50) :align-x :right :ink +red+))
    
    (incf y 70)
    
    ;; 5. Stroke Support
    (draw-text* stream "Stroke Support:" x y :align-y :top)
    (incf y 30)
    
    ;; Rectangle (Stroke)
    (draw-rectangle* stream x y (+ x 40) (+ y 30) :filled nil :ink +blue+ :line-thickness 2)
    (draw-text* stream "Rect (Outline)" (+ x 70) (+ y 15) :align-y :center)
    
    ;; Polygon (Stroke)
    (draw-polygon* stream (list (+ x 250) (+ y 30)
                                (+ x 270) y
                                (+ x 290) (+ y 30))
                   :filled nil :ink +red+ :line-thickness 3)
    (draw-text* stream "Poly (Outline)" (+ x 300) (+ y 15) :align-y :center)
    
    ;; Circle (Stroke)
    (draw-ellipse* stream (+ x 500) (+ y 15) 15 0 0 15 
                   :filled nil :ink +green+ :line-thickness 4)
    (draw-text* stream "Circle (Outline)" (+ x 530) (+ y 15) :align-y :center)

    (incf y 70)

    ;; 6. Robust Geometry (Glitch Fixes)
    (draw-text* stream "Robust Geometry:" x y :align-y :top)
    (incf y 30)

    ;; Rotated Rectangle (Should be a diamond, not a bounding box)
    (with-drawing-options (stream :transformation (make-rotation-transformation* (/ pi 4) (+ x 20) (+ y 20)))
      (draw-rectangle* stream x y (+ x 40) (+ y 40) :filled nil :ink +red+ :line-thickness 2))
    (draw-text* stream "Rotated Rect" (+ x 80) (+ y 20) :align-y :center)

    ;; Scaled Stroke (Should be thick)
    (with-drawing-options (stream :transformation (make-scaling-transformation 4.0 4.0))
      ;; Draw at 1/4th position so it lands in view. line-thickness 1 -> 4px visual
      (draw-line* stream (+ (/ x 4) 75) (+ (/ y 4) 5) (+ (/ x 4) 95) (+ (/ y 4) 5) 
                  :ink +blue+ :line-thickness 1))
    (draw-text* stream "Scaled Stroke" (+ x 390) (+ y 20) :align-y :center)

    ;; Flipped/Mirrored (Should still draw)
    (with-drawing-options (stream :transformation (clim:compose-transformations
                                                   (clim:make-translation-transformation (+ x 600) y)
                                                   (clim:make-scaling-transformation -1.0 1.0)))
      ;; Draw triangle pointing right, should point left after flip
      (draw-polygon* stream (list 0 0 20 10 0 20) :filled t :ink +purple+))
    (draw-text* stream "Flipped Poly" (+ x 620) (+ y 20) :align-y :center)

    (finish-output stream)))

(defmethod handle-event ((pane application-pane) (event key-press-event))
  (let ((frame (pane-frame pane))
        (key-name (keyboard-event-key-name event)))
    (when (member key-name '(:escape :q))
      (frame-exit frame))))


(defun run ()
  "Run the drawing demo using the sdl3/impeller backend."
  (setf clim:*default-server-path* '(:render-stack))
  (run-frame-top-level
   (make-application-frame 'drawing-demo
                           :width 1280
                           :height 720)))
