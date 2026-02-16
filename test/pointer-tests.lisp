;;;; pointer-tests.lisp — Unit tests for pointer handling

(in-package :mcclim-render-stack-tests)

(define-test render-stack-pointer-class-exists
  :parent mcclim-render-stack-suite
  "Test that render-stack-pointer class exists."
  (true (find-class 'mcclim-render-stack::render-stack-pointer nil)))

(define-test render-stack-pointer-can-be-created
  :parent mcclim-render-stack-suite
  "Test that render-stack-pointer can be instantiated."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    (true (typep pointer 'mcclim-render-stack::render-stack-pointer))))

(define-test pointer-position-returns-values
  :parent mcclim-render-stack-suite
  "Test pointer-position returns x and y values."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    (multiple-value-bind (x y)
        (mcclim-render-stack::pointer-position pointer)
      (is = 0 x)
      (is = 0 y))))

(define-test update-pointer-position-changes-position
  :parent mcclim-render-stack-suite
  "Test update-pointer-position function changes pointer position."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    (mcclim-render-stack::update-pointer-position pointer 150 250)
    (multiple-value-bind (x y)
        (mcclim-render-stack::pointer-position pointer)
      (is = 150 x)
      (is = 250 y))))

(define-test pointer-button-state-initial
  :parent mcclim-render-stack-suite
  "Test pointer-button-state returns initial state."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    (is = 0 (mcclim-render-stack::pointer-button-state pointer))))

(define-test update-pointer-button-state-left
  :parent mcclim-render-stack-suite
  "Test update-pointer-button-state with :left button."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    (mcclim-render-stack::update-pointer-button-state pointer :left t)
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-left-button+)))))

(define-test update-pointer-button-state-right
  :parent mcclim-render-stack-suite
  "Test update-pointer-button-state with :right button."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    (mcclim-render-stack::update-pointer-button-state pointer :right t)
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-right-button+)))))

(define-test update-pointer-button-state-middle
  :parent mcclim-render-stack-suite
  "Test update-pointer-button-state with :middle button."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    (mcclim-render-stack::update-pointer-button-state pointer :middle t)
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-middle-button+)))))

(define-test update-pointer-button-state-release
  :parent mcclim-render-stack-suite
  "Test releasing pointer button clears state."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    (mcclim-render-stack::update-pointer-button-state pointer :left t)
    (mcclim-render-stack::update-pointer-button-state pointer :left nil)
    (is = 0 (logand (mcclim-render-stack::pointer-button-state pointer)
                     clim:+pointer-left-button+))))

(define-test pointer-multi-button-tracking
  :parent mcclim-render-stack-suite
  "Test multiple buttons can be tracked simultaneously."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    (mcclim-render-stack::update-pointer-button-state pointer :left t)
    (mcclim-render-stack::update-pointer-button-state pointer :right t)
    (let ((state (mcclim-render-stack::pointer-button-state pointer)))
      (true (plusp (logand state clim:+pointer-left-button+)))
      (true (plusp (logand state clim:+pointer-right-button+))))))

(define-test update-pointer-position-function-exists
  :parent mcclim-render-stack-suite
  "Test update-pointer-position function exists."
  (true (fboundp 'mcclim-render-stack::update-pointer-position)))

(define-test update-pointer-button-state-function-exists
  :parent mcclim-render-stack-suite
  "Test update-pointer-button-state function exists."
  (true (fboundp 'mcclim-render-stack::update-pointer-button-state)))

(define-test pointer-button-state-method-exists
  :parent mcclim-render-stack-suite
  "Test pointer-button-state method exists for render-stack-pointer."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    (true (typep (mcclim-render-stack::pointer-button-state pointer) 'number))))
