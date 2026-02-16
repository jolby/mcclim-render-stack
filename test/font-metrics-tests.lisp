;;;; font-metrics-tests.lisp — Unit tests for font metrics protocol

(in-package :mcclim-render-stack-tests)

(define-test text-style-ascent-returns-numeric
  :parent mcclim-render-stack-suite
  "Test text-style-ascent returns numeric value."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port))
           (text-style (medium-text-style medium)))
      (let ((ascent (text-style-ascent text-style medium)))
        (is typep (numberp ascent) number)
        (is > ascent 0))
      (clim:destroy-port port))))

(define-test text-style-descent-returns-numeric
  :parent mcclim-render-stack-suite
  "Test text-style-descent returns numeric value."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port))
           (text-style (medium-text-style medium)))
      (let ((descent (text-style-descent text-style medium)))
        (is typep (numberp descent) number)
        (is > descent 0))
      (clim:destroy-port port))))

(define-test text-style-height-returns-numeric
  :parent mcclim-render-stack-suite
  "Test text-style-height returns numeric value."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port))
           (text-style (medium-text-style medium)))
      (let ((height (text-style-height text-style medium)))
        (is typep (numberp height) number)
        (is > height 0))
      (clim:destroy-port port))))

(define-test text-style-width-returns-numeric
  :parent mcclim-render-stack-suite
  "Test text-style-width returns numeric value."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port))
           (text-style (medium-text-style medium)))
      (let ((width (text-style-width text-style medium)))
        (is typep (numberp width) number)
        (is >= width 0))
      (clim:destroy-port port))))

(define-test text-size-returns-five-values
  :parent mcclim-render-stack-suite
  "Test text-size returns 5 values (width, height, final-x, final-y, baseline)."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port)))
      (multiple-value-bind (width height final-x final-y baseline)
          (text-size medium "Hello")
        (is typep (numberp width) number)
        (is typep (numberp height) number)
        (is typep (numberp final-x) number)
        (is typep (numberp final-y) number)
        (is typep (numberp baseline) number)
        (is > width 0)
        (is > height 0))
      (clim:destroy-port port))))

(define-test text-style-metrics-consistency
  :parent mcclim-render-stack-suite
  "Test that text-style-height >= text-style-ascent + text-style-descent."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port))
           (text-style (medium-text-style medium)))
      (let ((ascent (text-style-ascent text-style medium))
            (descent (text-style-descent text-style medium))
            (height (text-style-height text-style medium)))
        (is >= height (+ ascent descent)))
      (clim:destroy-port port))))
