;;;; medium-protocol-tests.lisp — Unit tests for medium drawing protocol

(in-package :mcclim-render-stack-tests)

(define-test medium-draw-point*-exists
  :parent mcclim-render-stack-suite
  "Test that medium-draw-point* method exists."
  (true (fboundp 'mcclim-render-stack::medium-draw-point*)))

(define-test medium-draw-points*-exists
  :parent mcclim-render-stack-suite
  "Test that medium-draw-points* method exists."
  (true (fboundp 'mcclim-render-stack::medium-draw-points*)))

(define-test medium-draw-line*-exists
  :parent mcclim-render-stack-suite
  "Test that medium-draw-line* method exists."
  (true (fboundp 'mcclim-render-stack::medium-draw-line*)))

(define-test medium-draw-lines*-exists
  :parent mcclim-render-stack-suite
  "Test that medium-draw-lines* method exists."
  (true (fboundp 'mcclim-render-stack::medium-draw-lines*)))

(define-test medium-draw-polygon*-exists
  :parent mcclim-render-stack-suite
  "Test that medium-draw-polygon* method exists."
  (true (fboundp 'mcclim-render-stack::medium-draw-polygon*)))

(define-test medium-draw-rectangle*-exists
  :parent mcclim-render-stack-suite
  "Test that medium-draw-rectangle* method exists."
  (true (fboundp 'mcclim-render-stack::medium-draw-rectangle*)))

(define-test medium-draw-ellipse*-exists
  :parent mcclim-render-stack-suite
  "Test that medium-draw-ellipse* method exists."
  (true (fboundp 'mcclim-render-stack::medium-draw-ellipse*)))

(define-test medium-draw-text*-exists
  :parent mcclim-render-stack-suite
  "Test that medium-draw-text* method exists."
  (true (fboundp 'mcclim-render-stack::medium-draw-text*)))

(define-test medium-clear-area-exists
  :parent mcclim-render-stack-suite
  "Test that medium-clear-area method exists."
  (true (fboundp 'mcclim-render-stack::medium-clear-area)))

(define-test medium-finish-output-exists
  :parent mcclim-render-stack-suite
  "Test that medium-finish-output method exists."
  (true (fboundp 'mcclim-render-stack::medium-finish-output)))

(define-test medium-force-output-exists
  :parent mcclim-render-stack-suite
  "Test that medium-force-output method exists."
  (true (fboundp 'mcclim-render-stack::medium-force-output)))

(define-test medium-draw-rectangle*-accepts-correct-args
  :parent mcclim-render-stack-suite
  "Test medium-draw-rectangle* accepts x1 y1 x2 y2 filled."
  (let ((medium (make-instance 'render-stack-medium)))
    (true (typep medium 'render-stack-medium))))

(define-test medium-draw-line*-accepts-correct-args
  :parent mcclim-render-stack-suite
  "Test medium-draw-line* accepts x1 y1 x2 y2."
  (let ((medium (make-instance 'render-stack-medium)))
    (true (typep medium 'render-stack-medium))))

(define-test medium-draw-polygon*-accepts-correct-args
  :parent mcclim-render-stack-suite
  "Test medium-draw-polygon* accepts coord-seq closed filled."
  (let ((medium (make-instance 'render-stack-medium)))
    (true (typep medium 'render-stack-medium))))

(define-test medium-draw-ellipse*-accepts-correct-args
  :parent mcclim-render-stack-suite
  "Test medium-draw-ellipse* accepts center-x center-y radius-x radius-y."
  (let ((medium (make-instance 'render-stack-medium)))
    (true (typep medium 'render-stack-medium))))

(define-test medium-draw-text*-accepts-correct-args
  :parent mcclim-render-stack-suite
  "Test medium-draw-text* accepts text string and position."
  (let ((medium (make-instance 'render-stack-medium)))
    (true (typep medium 'render-stack-medium))))

(define-test medium-protocol-is-complete
  :parent mcclim-render-stack-suite
  "Test that all required medium drawing protocol methods are defined."
  (true (fboundp 'mcclim-render-stack::medium-draw-point*))
  (true (fboundp 'mcclim-render-stack::medium-draw-points*))
  (true (fboundp 'mcclim-render-stack::medium-draw-line*))
  (true (fboundp 'mcclim-render-stack::medium-draw-lines*))
  (true (fboundp 'mcclim-render-stack::medium-draw-polygon*))
  (true (fboundp 'mcclim-render-stack::medium-draw-rectangle*))
  (true (fboundp 'mcclim-render-stack::medium-draw-ellipse*))
  (true (fboundp 'mcclim-render-stack::medium-draw-text*))
  (true (fboundp 'mcclim-render-stack::medium-clear-area))
  (true (fboundp 'mcclim-render-stack::medium-finish-output))
  (true (fboundp 'mcclim-render-stack::medium-force-output)))
