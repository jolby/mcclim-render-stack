;;;; example/gradient-demo.lisp
;;;; Visual demo of gradient designs using standard CLIM APIs

(defpackage :impeller-gradient-demo
  (:use :clim :clim-lisp)
  (:export #:run))

(in-package :impeller-gradient-demo)

;;; Demo application using standard CLIM drawing functions with gradient inks

(define-application-frame gradient-demo ()
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
        (labelling (:label "Gradient Demo - Standard CLIM API")
          (outlining (:thickness 10)
            canvas)))))))

(defun display-canvas (frame stream)
  (declare (ignore frame))
  (let ((x 20) (y 20))

    ;; Section 1: Linear Gradients
    (draw-text* stream "Linear Gradients:" x y :align-y :top
                :text-style (make-text-style :sans-serif :bold :large))
    (incf y 35)

    ;; Horizontal gradient (red -> blue)
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons x y) (cons (+ x 150) y)
                 (list +red+ +blue+))))
      (draw-rectangle* stream x y (+ x 150) (+ y 60) :ink grad :filled t))
    (draw-text* stream "Horizontal" (+ x 160) (+ y 25) :align-y :center)

    ;; Vertical gradient (green -> yellow)
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons (+ x 250) y) (cons (+ x 250) (+ y 60))
                 (list +green+ +yellow+))))
      (draw-rectangle* stream (+ x 250) y (+ x 400) (+ y 60) :ink grad :filled t))
    (draw-text* stream "Vertical" (+ x 410) (+ y 25) :align-y :center)

    ;; Diagonal gradient (purple -> cyan)
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons (+ x 500) y) (cons (+ x 650) (+ y 60))
                 (list +purple+ +cyan+))))
      (draw-rectangle* stream (+ x 500) y (+ x 650) (+ y 60) :ink grad :filled t))
    (draw-text* stream "Diagonal" (+ x 660) (+ y 25) :align-y :center)

    (incf y 90)

    ;; Section 2: Multi-stop gradients
    (draw-text* stream "Multi-Stop Gradients:" x y :align-y :top
                :text-style (make-text-style :sans-serif :bold :large))
    (incf y 35)

    ;; Rainbow gradient
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons x y) (cons (+ x 300) y)
                 (list +red+ +orange+ +yellow+ +green+ +blue+ +purple+))))
      (draw-rectangle* stream x y (+ x 300) (+ y 60) :ink grad :filled t))
    (draw-text* stream "Rainbow (6 stops)" (+ x 310) (+ y 25) :align-y :center)

    ;; Explicit stop positions
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons (+ x 450) y) (cons (+ x 750) y)
                 (list (cons 0.0 +red+)
                       (cons 0.2 +yellow+)
                       (cons 0.8 +yellow+)
                       (cons 1.0 +red+)))))
      (draw-rectangle* stream (+ x 450) y (+ x 750) (+ y 60) :ink grad :filled t))
    (draw-text* stream "Explicit stops" (+ x 760) (+ y 25) :align-y :center)

    (incf y 90)

    ;; Section 3: Radial Gradients
    (draw-text* stream "Radial Gradients:" x y :align-y :top
                :text-style (make-text-style :sans-serif :bold :large))
    (incf y 35)

    ;; Simple radial (white center -> black edge)
    (let* ((cx (+ x 60))
           (cy (+ y 60))
           (grad (mcclim-sdl3-impeller-backend:make-radial-gradient
                  (cons cx cy) 60
                  (list +white+ +black+))))
      (draw-ellipse* stream cx cy 60 0 0 60 :ink grad :filled t))
    (draw-text* stream "Simple" (+ x 130) (+ y 60) :align-y :center)

    ;; Colored radial (red center -> blue edge)
    (let* ((cx (+ x 280))
           (cy (+ y 60))
           (grad (mcclim-sdl3-impeller-backend:make-radial-gradient
                  (cons cx cy) 60
                  (list +red+ +blue+))))
      (draw-ellipse* stream cx cy 60 0 0 60 :ink grad :filled t))
    (draw-text* stream "Colored" (+ x 350) (+ y 60) :align-y :center)

    ;; Multi-stop radial (sunburst)
    (let* ((cx (+ x 500))
           (cy (+ y 60))
           (grad (mcclim-sdl3-impeller-backend:make-radial-gradient
                  (cons cx cy) 60
                  (list +yellow+ +orange+ +red+ +dark-red+))))
      (draw-ellipse* stream cx cy 60 0 0 60 :ink grad :filled t))
    (draw-text* stream "Sunburst" (+ x 570) (+ y 60) :align-y :center)

    (incf y 150)

    ;; Section 4: Tile Modes
    (draw-text* stream "Tile Modes:" x y :align-y :top
                :text-style (make-text-style :sans-serif :bold :large))
    (incf y 35)

    ;; Clamp (default) - gradient is 40px wide, rect is 150px
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons (+ x 30) y) (cons (+ x 70) y)
                 (list +red+ +blue+)
                 :tile-mode :clamp)))
      (draw-rectangle* stream x y (+ x 150) (+ y 50) :ink grad :filled t))
    (draw-text* stream ":clamp" (+ x 160) (+ y 20) :align-y :center)

    ;; Repeat
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons (+ x 230) y) (cons (+ x 270) y)
                 (list +red+ +blue+)
                 :tile-mode :repeat)))
      (draw-rectangle* stream (+ x 200) y (+ x 350) (+ y 50) :ink grad :filled t))
    (draw-text* stream ":repeat" (+ x 360) (+ y 20) :align-y :center)

    ;; Mirror
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons (+ x 430) y) (cons (+ x 470) y)
                 (list +red+ +blue+)
                 :tile-mode :mirror)))
      (draw-rectangle* stream (+ x 400) y (+ x 550) (+ y 50) :ink grad :filled t))
    (draw-text* stream ":mirror" (+ x 560) (+ y 20) :align-y :center)

    (incf y 80)

    ;; Section 5: Gradients on different shapes
    (draw-text* stream "Gradient Shapes:" x y :align-y :top
                :text-style (make-text-style :sans-serif :bold :large))
    (incf y 35)

    ;; Square
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons x y) (cons (+ x 80) (+ y 80))
                 (list +cyan+ +magenta+))))
      (draw-rectangle* stream x y (+ x 80) (+ y 80) :ink grad :filled t))
    (draw-text* stream "Square" (+ x 40) (+ y 90) :align-x :center)

    ;; Wide rectangle
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons (+ x 120) y) (cons (+ x 320) y)
                 (list +green+ +blue+))))
      (draw-rectangle* stream (+ x 120) y (+ x 320) (+ y 80) :ink grad :filled t))
    (draw-text* stream "Wide" (+ x 220) (+ y 90) :align-x :center)

    ;; Tall rectangle
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons (+ x 360) y) (cons (+ x 360) (+ y 80))
                 (list +orange+ +purple+))))
      (draw-rectangle* stream (+ x 360) y (+ x 420) (+ y 80) :ink grad :filled t))
    (draw-text* stream "Tall" (+ x 390) (+ y 90) :align-x :center)

    ;; Ellipse with radial gradient
    (let* ((cx (+ x 520))
           (cy (+ y 40))
           (grad (mcclim-sdl3-impeller-backend:make-radial-gradient
                  (cons cx cy) 60
                  (list +white+ +dark-blue+))))
      (draw-ellipse* stream cx cy 80 0 0 40 :ink grad :filled t))
    (draw-text* stream "Ellipse" (+ x 520) (+ y 90) :align-x :center)

    ;; Polygon with gradient
    (let ((grad (mcclim-sdl3-impeller-backend:make-linear-gradient
                 (cons (+ x 620) y) (cons (+ x 720) (+ y 80))
                 (list +gold+ +dark-green+))))
      (draw-polygon* stream (list (+ x 670) y
                                  (+ x 720) (+ y 40)
                                  (+ x 670) (+ y 80)
                                  (+ x 620) (+ y 40))
                     :ink grad :filled t :closed t))
    (draw-text* stream "Diamond" (+ x 670) (+ y 90) :align-x :center)

    (finish-output stream)))

(defmethod handle-event ((pane application-pane) (event key-press-event))
  (let ((frame (pane-frame pane))
        (key-name (keyboard-event-key-name event)))
    (when (member key-name '(:escape :q))
      (frame-exit frame))))

(defun run ()
  "Run the gradient demo using the sdl3/impeller backend."
  (setf clim:*default-server-path* '(:sdl3-impeller))
  (run-frame-top-level
   (make-application-frame 'gradient-demo
                           :width 900
                           :height 650)))
