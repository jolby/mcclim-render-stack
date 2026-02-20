;;;; medium-protocol-tests.lisp — Unit tests for medium drawing protocol

(in-package :mcclim-render-stack-tests)

(define-test medium-creation
  :parent mcclim-render-stack-suite
  "Test basic medium creation."
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
