;;;; Tests for port implementation
;;;; Uses Parachute testing framework

(in-package :mcclim-render-stack-tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(define-test port-suite
  :parent mcclim-render-stack-suite
  :description "Test suite for render-stack CLIM port implementation")

(define-test port-requires-active-runner
  :parent port-suite
  :description "Verify port creation fails without an active runner."
  ;; Without *runner* bound, port creation should error
  (let ((rs-internals:*runner* nil))
    (fail (make-instance 'mcclim-render-stack::render-stack-port)
          'error)))
