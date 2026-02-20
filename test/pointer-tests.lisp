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

(define-test mouse-button-translation
  :parent mcclim-render-stack-suite
  "Test SDL3 to CLIM mouse button mapping."
  ;; Test button mapping
  (is eq :left (mcclim-render-stack::sdl3-button-to-clim-button 1))
  (is eq :middle (mcclim-render-stack::sdl3-button-to-clim-button 2))
  (is eq :right (mcclim-render-stack::sdl3-button-to-clim-button 3))
  (is eq :wheel-up (mcclim-render-stack::sdl3-button-to-clim-button 4))
  (is eq :wheel-down (mcclim-render-stack::sdl3-button-to-clim-button 5))
  ;; Test unknown button returns nil
  (is eq nil (mcclim-render-stack::sdl3-button-to-clim-button 99))
  ;; Test button constants
  (is = clim:+pointer-left-button+ (mcclim-render-stack::sdl3-button-to-clim-constant 1))
  (is = clim:+pointer-middle-button+ (mcclim-render-stack::sdl3-button-to-clim-constant 2))
  (is = clim:+pointer-right-button+ (mcclim-render-stack::sdl3-button-to-clim-constant 3)))

(define-test pointer-button-state-tracking
  :parent mcclim-render-stack-suite
  "Test pointer button state tracking."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    ;; Initial state should be no button
    (is = 0 (mcclim-render-stack::pointer-button-state pointer))
    ;; Press left button
    (mcclim-render-stack::update-pointer-button-state pointer :left t)
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-left-button+)))
    ;; Press right button (multi-button)
    (mcclim-render-stack::update-pointer-button-state pointer :right t)
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-left-button+)))
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-right-button+)))
    ;; Release left button
    (mcclim-render-stack::update-pointer-button-state pointer :left nil)
    (is = 0 (logand (mcclim-render-stack::pointer-button-state pointer)
                    clim:+pointer-left-button+))
    ;; Right button should still be pressed
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-right-button+)))))

;;; Pointer Button State Tracking Test

(define-test pointer-button-state-tracking
  :parent mcclim-render-stack-suite
  "Test pointer button state tracking."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    ;; Initial state should be no button
    (is = 0 (mcclim-render-stack::pointer-button-state pointer))
    ;; Press left button
    (mcclim-render-stack::update-pointer-button-state pointer :left t)
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-left-button+)))
    ;; Press right button (multi-button)
    (mcclim-render-stack::update-pointer-button-state pointer :right t)
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-left-button+)))
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-right-button+)))
    ;; Release left button
    (mcclim-render-stack::update-pointer-button-state pointer :left nil)
    (is = 0 (logand (mcclim-render-stack::pointer-button-state pointer)
                    clim:+pointer-left-button+))
    ;; Right button should still be pressed
    (true (plusp (logand (mcclim-render-stack::pointer-button-state pointer)
                         clim:+pointer-right-button+)))))

(define-test pointer-position-tracking
  :parent mcclim-render-stack-suite
  "Test pointer position tracking."
  (let ((pointer (make-instance 'mcclim-render-stack::render-stack-pointer)))
    ;; Initial position should be 0,0
    (multiple-value-bind (x y) (mcclim-render-stack::pointer-position pointer)
      (is = 0 x)
      (is = 0 y))
    ;; Update position
    (mcclim-render-stack::update-pointer-position pointer 100 200)
    (multiple-value-bind (x y) (mcclim-render-stack::pointer-position pointer)
      (is = 100 x)
      (is = 200 y))
    ;; Update again
    (mcclim-render-stack::update-pointer-position pointer 300 400)
    (multiple-value-bind (x y) (mcclim-render-stack::pointer-position pointer)
      (is = 300 x)
      (is = 400 y))))
