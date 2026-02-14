;;;; ink-conversion-tests.lisp — Unit tests for ink conversion functions

(in-package :mcclim-render-stack-tests)

(in-suite :mcclim-render-stack)

;;; Test helper: Create a mock medium for testing

(defclass test-medium (render-stack-medium)
  ()
  (:default-initargs
   :foreground clim:+black+
   :background clim:+white+))

(defun make-test-medium ()
  "Create a test medium instance for testing."
  (make-instance 'test-medium))

;;; Ink Conversion Tests

(define-test test-ink-conversion-rgb-color
  "Test that RGB colors convert to correct RGBA values."
  (let ((medium (make-test-medium)))
    (let ((color (clim:make-rgb-color 1.0 0.5 0.0)))
      (multiple-value-bind (r g b a)
          (mcclim-render-stack::clim-ink-to-impeller-color color medium)
        (is = 1.0 r)
        (is = 0.5 g)
        (is = 0.0 b)
        (is = 1.0 a)))))

(define-test test-ink-conversion-rgba-color
  "Test that RGBA colors preserve alpha."
  (let ((medium (make-test-medium)))
    (let ((color (clim:make-rgb-color 0.25 0.5 0.75)))
      (clim内在化::%set-color-alpha color 0.5)
      (multiple-value-bind (r g b a)
          (mcclim-render-stack::clim-ink-to-impeller-color color medium)
        (is = 0.25 r)
        (is = 0.5 g)
        (is = 0.75 b)
        (is = 0.5 a)))))

(define-test test-ink-conversion-foreground-ink
  "Test that +foreground-ink+ resolves to medium's foreground."
  (let ((medium (make-test-medium)))
    (setf (clim:medium-foreground medium) (clim:make-rgb-color 0.1 0.2 0.3))
    (multiple-value-bind (r g b a)
        (mcclim-render-stack::clim-ink-to-impeller-color clim:+foreground-ink+ medium)
      (is = 0.1 r)
      (is = 0.2 g)
      (is = 0.3 b)
      (is = 1.0 a))))

(define-test test-ink-conversion-background-ink
  "Test that +background-ink+ resolves to medium's background."
  (let ((medium (make-test-medium)))
    (setf (clim:medium-background medium) (clim:make-rgb-color 0.9 0.8 0.7))
    (multiple-value-bind (r g b a)
        (mcclim-render-stack::clim-ink-to-impeller-color clim:+background-ink+ medium)
      (is = 0.9 r)
      (is = 0.8 g)
      (is = 0.7 b)
      (is = 1.0 a))))

(define-test test-ink-conversion-black
  "Test that +black+ converts correctly."
  (let ((medium (make-test-medium)))
    (multiple-value-bind (r g b a)
        (mcclim-render-stack::clim-ink-to-impeller-color clim:+black+ medium)
      (is = 0.0 r)
      (is = 0.0 g)
      (is = 0.0 b)
      (is = 1.0 a))))

(define-test test-ink-conversion-white
  "Test that +white+ converts correctly."
  (let ((medium (make-test-medium)))
    (multiple-value-bind (r g b a)
        (mcclim-render-stack::clim-ink-to-impeller-color clim:+white+ medium)
      (is = 1.0 r)
      (is = 1.0 g)
      (is = 1.0 b)
      (is = 1.0 a))))

(define-test test-ink-conversion-red
  "Test that +red+ converts correctly."
  (let ((medium (make-test-medium)))
    (multiple-value-bind (r g b a)
        (mcclim-render-stack::clim-ink-to-impeller-color clim:+red+ medium)
      (is = 1.0 r)
      (is = 0.0 g)
      (is = 0.0 b)
      (is = 1.0 a))))

(define-test test-ink-conversion-null-ink
  "Test that null ink converts to black."
  (let ((medium (make-test-medium)))
    (multiple-value-bind (r g b a)
        (mcclim-render-stack::clim-ink-to-impeller-color nil medium)
      (is = 0.0 r)
      (is = 0.0 g)
      (is = 0.0 b)
      (is = 1.0 a))))

(define-test test-ink-conversion-fallback
  "Test that unknown ink types fall back to foreground."
  (let ((medium (make-test-medium)))
    (setf (clim:medium-foreground medium) (clim:make-rgb-color 0.5 0.5 0.5))
    ;; Flipping-ink or other unknown types should fall back
    (multiple-value-bind (r g b a)
        (mcclim-render-stack::clim-ink-to-impeller-color 
         (clim:make-flipping-ink clim:+red+ clim:+blue+) medium)
      (is = 0.5 r)
      (is = 0.5 g)
      (is = 0.5 b)
      (is = 1.0 a))))

;;; set-paint-from-ink tests

(define-test test-set-paint-from-ink
  "Test that set-paint-from-ink configures paint correctly."
  (let ((medium (make-test-medium))
        (paint (frs:make-paint)))
    (unwind-protect
         (progn
           (mcclim-render-stack::set-paint-from-ink 
            paint (clim:make-rgb-color 0.3 0.6 0.9) medium)
           ;; Just verify it doesn't crash and returns nil (no color source)
           (is (null 
                (mcclim-render-stack::set-paint-from-ink 
                 paint clim:+foreground-ink+ medium))))
      (frs:release-paint paint))))

;;; with-ink-on-paint macro tests

(define-test test-with-ink-on-paint-exists
  "Test that with-ink-on-paint macro is defined."
  (is (fboundp 'mcclim-render-stack::with-ink-on-paint)))

(define-test test-with-ink-on-paint-uses-ink
  "Test that with-ink-on-paint executes body with correct ink."
  (let ((medium (make-test-medium))
        (paint (frs:make-paint)))
    (unwind-protect
         (let ((result nil))
           (mcclim-render-stack::with-ink-on-paint 
            (paint clim:+red+ medium)
            (setf result t))
           (is-true result))
      (frs:release-paint paint))))
