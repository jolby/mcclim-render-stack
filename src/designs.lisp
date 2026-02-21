(in-package :mcclim-render-stack)

;;; Gradient and shadow design classes extending CLIM's design protocol

;;; Design classes

(defclass linear-gradient (clim:design)
  ((start-point :initarg :start-point
                :reader linear-gradient-start-point)
   (end-point :initarg :end-point
              :reader linear-gradient-end-point)
   (color-stops :initarg :color-stops
                :reader linear-gradient-color-stops
                :documentation "List of (position . color) pairs, 0.0 <= position <= 1.0")
   (tile-mode :initarg :tile-mode
              :reader linear-gradient-tile-mode
              :initform :clamp
              :documentation ":clamp, :repeat, :mirror, or :decal")))

(defclass radial-gradient (clim:design)
  ((center :initarg :center
           :reader radial-gradient-center)
   (radius :initarg :radius
           :reader radial-gradient-radius)
   (color-stops :initarg :color-stops
                :reader radial-gradient-color-stops
                :documentation "List of (position . color) pairs, 0.0 <= position <= 1.0")
   (tile-mode :initarg :tile-mode
              :reader radial-gradient-tile-mode
              :initform :clamp
              :documentation ":clamp, :repeat, :mirror, or :decal")))

(defclass drop-shadow (clim:design)
  ((ink :initarg :ink
        :reader drop-shadow-ink)
   (offset-x :initarg :offset-x
             :reader drop-shadow-offset-x
             :initform 3.0)
   (offset-y :initarg :offset-y
             :reader drop-shadow-offset-y
             :initform 3.0)
   (blur-sigma :initarg :blur-sigma
               :reader drop-shadow-blur-sigma
               :initform 2.0)
   (shadow-color :initarg :shadow-color
                 :reader drop-shadow-shadow-color
                 :initform (clim:make-rgb-color 0.0f0 0.0f0 0.0f0))))

(defclass elevated-shadow (clim:design)
  ((elevation :initarg :elevation
              :reader elevated-shadow-elevation
              :initform 4.0
              :type real)
   (corner-radius :initarg :corner-radius
                  :reader elevated-shadow-corner-radius
                  :initform 0.0
                  :type real)
   (shadow-color :initarg :shadow-color
                 :reader elevated-shadow-shadow-color
                 :initform (clim:make-rgb-color 0.0f0 0.0f0 0.0f0))
   (shadow-alpha :initarg :shadow-alpha
                 :reader elevated-shadow-shadow-alpha
                 :initform 0.25
                 :type real)))

;;; Constructors

(defun make-linear-gradient (start-point end-point color-stops
                             &key (tile-mode :clamp))
  "Create a linear gradient design.

Arguments:
  start-point  - (x1 y1) list or (x1 . y1) cons
  end-point    - (x2 y2) list or (x2 . y2) cons
  color-stops  - List of (position . color) pairs, 0.0 <= position <= 1.0
  tile-mode    - :clamp (default), :repeat, :mirror, :decal"
  (make-instance 'linear-gradient
                 :start-point start-point
                 :end-point end-point
                 :color-stops (normalize-color-stops color-stops)
                 :tile-mode tile-mode))

(defun make-radial-gradient (center radius color-stops
                             &key (tile-mode :clamp))
  "Create a radial gradient design.

Arguments:
  center       - (cx cy) list or (cx . cy) cons
  radius       - Gradient radius in pixels
  color-stops  - List of (position . color) pairs
  tile-mode    - :clamp (default), :repeat, :mirror, :decal"
  (make-instance 'radial-gradient
                 :center center
                 :radius radius
                 :color-stops (normalize-color-stops color-stops)
                 :tile-mode tile-mode))

(defun make-drop-shadow (ink &key (offset-x 3.0) (offset-y 3.0)
                                  (blur-sigma 2.0)
                                  (shadow-color (clim:make-rgb-color 0.0f0 0.0f0 0.0f0)))
  "Create a drop shadow design wrapping an inner design."
  (make-instance 'drop-shadow
                 :ink ink
                 :offset-x offset-x
                 :offset-y offset-y
                 :blur-sigma blur-sigma
                 :shadow-color shadow-color))

(defun make-elevated-shadow (&key (elevation 4.0)
                                  (corner-radius 0.0)
                                  (shadow-color (clim:make-rgb-color 0.0f0 0.0f0 0.0f0))
                                  (shadow-alpha 0.25))
  "Create a Material Design elevation shadow design."
  (make-instance 'elevated-shadow
                 :elevation (float elevation 1.0f0)
                 :corner-radius (float corner-radius 1.0f0)
                 :shadow-color shadow-color
                 :shadow-alpha (float shadow-alpha 1.0f0)))

;;; Color stop normalization

(defun normalize-color-stops (color-stops)
  "Validate and normalize color-stop list.

Input formats:
  - ((0.0 . color) (0.5 . color) (1.0 . color)) - explicit positions
  - (color color color) - auto-position evenly
  - ((0.0 color) (0.5 color) (1.0 color)) - pairs with positions

Output: Sorted list of (position . color), positions 0.0-1.0"
  (cond
    ((null color-stops)
     (error "color-stops must not be empty"))
    ((and (or (listp (first color-stops))
              (consp (first color-stops)))
          (numberp (car (first color-stops))))
     (%normalize-explicit-stops color-stops))
    ((and (listp (first color-stops))
          (not (numberp (car (first color-stops)))))
     (%normalize-auto-distributed-stops (mapcar #'first color-stops)))
    ((typep (first color-stops) 'clim:design)
     (%normalize-auto-distributed-stops color-stops))
    (t
     (error "Invalid color-stops format: ~S" color-stops))))

(defun %normalize-explicit-stops (stops)
  "Normalize stops in ((position color) ...) or ((position . color) ...) format."
  (let ((normalized
          (mapcar (lambda (stop)
                    (let ((pos (car stop)))
                      (unless (numberp pos)
                        (error "Stop position must be a number: ~S" pos))
                      (cons (float pos 1.0f0) (cdr stop))))
                  stops)))
    (dolist (stop normalized)
      (let ((pos (car stop)))
        (unless (and (>= pos 0.0f0) (<= pos 1.0f0))
          (error "Color stop position out of range [0.0, 1.0]: ~F" pos))))
    (sort normalized #'< :key #'car)))

(defun %normalize-auto-distributed-stops (colors)
  "Distribute colors evenly across [0.0, 1.0]."
  (let ((n (length colors)))
    (cond
      ((= n 1)
       (let ((color (first colors)))
         (list (cons 0.0f0 color) (cons 1.0f0 color))))
      ((= n 2)
       (list (cons 0.0f0 (first colors))
             (cons 1.0f0 (second colors))))
      (t
       (let ((step (/ 1.0f0 (1- n))))
         (loop for i from 0 below n
               collect (cons (float (* i step) 1.0f0) (nth i colors))))))))

;;; Point extraction helpers

(defun %point-x (point)
  "Extract X coordinate from a point (either (x y) list or (x . y) cons)."
  (car point))

(defun %point-y (point)
  "Extract Y coordinate from a point (either (x y) list or (x . y) cons)."
  (let ((rest (cdr point)))
    (if (listp rest)
        (car rest)
        rest)))

;;; Impeller color source creation from gradient designs

(defun %clim-color-to-rgba-list (color)
  "Convert a CLIM color to (r g b a) list for Impeller."
  (multiple-value-bind (r g b a)
      (clime:color-rgba color)
    (list (float r 1.0f0) (float g 1.0f0) (float b 1.0f0) (float a 1.0f0))))

(defun %make-color-source-from-gradient (gradient)
  "Create an frs color source from a gradient design.
Returns a color source pointer (caller must release), or NIL if not a gradient."
  (typecase gradient
    (linear-gradient
     (let* ((start  (linear-gradient-start-point gradient))
            (end    (linear-gradient-end-point gradient))
            (stops  (linear-gradient-color-stops gradient))
            (colors (mapcar (lambda (cs) (%clim-color-to-rgba-list (cdr cs))) stops))
            (poses  (mapcar #'car stops)))
       (frs:make-linear-gradient-color-source
        (float (%point-x start) 1.0f0) (float (%point-y start) 1.0f0)
        (float (%point-x end)   1.0f0) (float (%point-y end)   1.0f0)
        colors poses
        :tile-mode (linear-gradient-tile-mode gradient))))
    (radial-gradient
     (let* ((center (radial-gradient-center gradient))
            (radius (radial-gradient-radius gradient))
            (stops  (radial-gradient-color-stops gradient))
            (colors (mapcar (lambda (cs) (%clim-color-to-rgba-list (cdr cs))) stops))
            (poses  (mapcar #'car stops)))
       (frs:make-radial-gradient-color-source
        (float (%point-x center) 1.0f0) (float (%point-y center) 1.0f0)
        (float radius 1.0f0)
        colors poses
        :tile-mode (radial-gradient-tile-mode gradient))))
    (t nil)))

;;; CLIM design protocol methods

(defmethod clime:design-ink ((design linear-gradient) x y)
  "Compute ink at point (x, y) for a linear gradient."
  (let* ((start (linear-gradient-start-point design))
         (end   (linear-gradient-end-point design))
         (stops (linear-gradient-color-stops design))
         (x1 (%point-x start)) (y1 (%point-y start))
         (x2 (%point-x end))   (y2 (%point-y end))
         (dx (- x2 x1)) (dy (- y2 y1))
         (px (- x x1))  (py (- y y1))
         (dot-self (+ (* dx dx) (* dy dy)))
         (t-raw (if (> dot-self 0.0)
                    (/ (+ (* px dx) (* py dy)) dot-self)
                    0.0)))
    (%interpolate-color-stops stops
                              (%apply-tile-mode t-raw (linear-gradient-tile-mode design)))))

(defmethod clime:design-ink ((design radial-gradient) x y)
  "Compute ink at point (x, y) for a radial gradient."
  (let* ((center (radial-gradient-center design))
         (radius (radial-gradient-radius design))
         (cx (%point-x center)) (cy (%point-y center))
         (dx (- x cx)) (dy (- y cy))
         (t-raw (if (> radius 0.0) (/ (sqrt (+ (* dx dx) (* dy dy))) radius) 0.0)))
    (%interpolate-color-stops (radial-gradient-color-stops design)
                              (%apply-tile-mode t-raw (radial-gradient-tile-mode design)))))

(defmethod clime:design-ink ((design drop-shadow) x y)
  (clime:design-ink (drop-shadow-ink design) x y))

(defmethod clime:design-ink ((design elevated-shadow) x y)
  (declare (ignore x y))
  (multiple-value-bind (r g b)
      (clim:color-rgb (elevated-shadow-shadow-color design))
    (make-rgba-color r g b (elevated-shadow-shadow-alpha design))))

(defmethod clime:color-rgba ((design linear-gradient))
  (values 0.5f0 0.5f0 0.5f0 0.5f0))

(defmethod clime:color-rgba ((design radial-gradient))
  (values 0.5f0 0.5f0 0.5f0 0.5f0))

(defmethod clime:color-rgba ((design drop-shadow))
  (values 0.5f0 0.5f0 0.5f0 0.5f0))

(defmethod clime:color-rgba ((design elevated-shadow))
  (multiple-value-bind (r g b)
      (clim:color-rgb (elevated-shadow-shadow-color design))
    (values (float r 1.0f0) (float g 1.0f0) (float b 1.0f0)
            (float (elevated-shadow-shadow-alpha design) 1.0f0))))

;;; Color stop helpers

(defun %apply-tile-mode (t-raw tile-mode)
  (case tile-mode
    (:clamp  (max 0.0f0 (min 1.0f0 t-raw)))
    (:repeat (- t-raw (floor t-raw)))
    (:mirror (let ((t-w (- t-raw (floor t-raw))))
               (if (evenp (floor t-raw)) t-w (- 1.0f0 t-w))))
    (:decal  (if (and (>= t-raw 0.0f0) (<= t-raw 1.0f0)) t-raw -1.0f0))
    (t       (max 0.0f0 (min 1.0f0 t-raw)))))

(defun %interpolate-color-stops (stops t-val)
  (cond
    ((< t-val 0.0f0) (cdar stops))
    ((> t-val 1.0f0) (cdar (last stops)))
    (t
     (let* ((before (find-if (lambda (s) (<= (car s) t-val)) stops :from-end t))
            (after  (find-if (lambda (s) (>= (car s) t-val)) stops)))
       (cond
         ((null before) (cdar stops))
         ((null after)  (cdar (last stops)))
         ((= (car before) (car after)) (cdr after))
         (t
          (let ((alpha (/ (- t-val (car before)) (- (car after) (car before)))))
            (%interpolate-colors (cdr before) (cdr after) alpha))))))))

(defun %interpolate-colors (c1 c2 alpha)
  (multiple-value-bind (r1 g1 b1 a1) (clime:color-rgba c1)
    (multiple-value-bind (r2 g2 b2 a2) (clime:color-rgba c2)
      (make-rgba-color
       (+ (* r1 (- 1.0f0 alpha)) (* r2 alpha))
       (+ (* g1 (- 1.0f0 alpha)) (* g2 alpha))
       (+ (* b1 (- 1.0f0 alpha)) (* b2 alpha))
       (+ (* a1 (- 1.0f0 alpha)) (* a2 alpha))))))
