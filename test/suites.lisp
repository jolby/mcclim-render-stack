(in-package :mcclim-render-stack-tests)


;;; ============================================================================
;;; Test Suites
;;; ============================================================================

;; Root suite
(define-test mcclim-render-stack-suite
  :parent NIL
  :description "Top-level test suite for mcclim-render-stack backend.")

(define-test multi-window-delegate-suite
  :parent mcclim-render-stack-suite
  :description "Test suite for multi-window render delegate implementation."
  )

(define-test window-registration-suite
  :parent multi-window-delegate-suite
  :description   "Tests for window registration protocol.")

(define-test protocol-methods-suite
  :parent multi-window-delegate-suite
  :description   "Tests for render-delegate protocol methods.")

;; Task 1.5: Global Engine Infrastructure
(define-test global-engine-suite
  :parent mcclim-render-stack-suite
  :description "Test suite for global engine infrastructure.")

;; Task 1.6: Port Refactor
(define-test port-refactor-suite
  :parent mcclim-render-stack-suite
  :description "Test suite for port refactor.")
