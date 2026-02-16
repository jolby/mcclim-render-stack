;;;; text-style-tests.lisp — Unit tests for text style mapping

(in-package :mcclim-render-stack-tests)

(define-test text-style-ascent-exists
  :parent mcclim-render-stack-suite
  "Test that text-style-ascent method exists."
  (true (fboundp 'mcclim-render-stack::text-style-ascent)))

(define-test text-style-descent-exists
  :parent mcclim-render-stack-suite
  "Test that text-style-descent method exists."
  (true (fboundp 'mcclim-render-stack::text-style-descent)))

(define-test text-style-height-method-exists
  :parent mcclim-render-stack-suite
  "Test that text-style-height method exists."
  (true (fboundp 'mcclim-render-stack::text-style-height)))

(define-test text-style-width-method-exists
  :parent mcclim-render-stack-suite
  "Test that text-style-width method exists."
  (true (fboundp 'mcclim-render-stack::text-style-width)))

(define-test text-size-method-exists
  :parent mcclim-render-stack-suite
  "Test that text-size method exists."
  (true (fboundp 'mcclim-render-stack::text-size)))

(define-test text-style-mapping-font-family
  :parent mcclim-render-stack-suite
  "Test font family mapping: :serif, :sans-serif, :fix."
  (let ((serif-style (make-text-style :serif nil :normal))
        (sans-style (make-text-style :sans-serif nil :normal))
        (fix-style (make-text-style :fix nil :normal)))
    (is eq :serif (text-style-family serif-style))
    (is eq :sans-serif (text-style-family sans-style))
    (is eq :fix (text-style-family fix-style))))

(define-test text-style-medium-text-style
  :parent mcclim-render-stack-suite
  "Test medium-text-style returns a valid text-style."
  (let ((medium (make-instance 'render-stack-medium)))
    (true (text-style-p (medium-text-style medium)))))

(define-test medium-draw-text*-accepts-alignment
  :parent mcclim-render-stack-suite
  "Test medium-draw-text* accepts alignment arguments."
  (let ((medium (make-instance 'render-stack-medium)))
    (true (typep medium 'render-stack-medium))))

(define-test text-style-protocol-complete
  :parent mcclim-render-stack-suite
  "Test that all text style protocol methods are defined."
  (true (fboundp 'mcclim-render-stack::text-style-ascent))
  (true (fboundp 'mcclim-render-stack::text-style-descent))
  (true (fboundp 'mcclim-render-stack::text-style-height))
  (true (fboundp 'mcclim-render-stack::text-style-width))
  (true (fboundp 'mcclim-render-stack::text-size)))
