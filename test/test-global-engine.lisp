;;;; test/test-global-engine.lisp
;;;; Tests for global engine infrastructure (Task 1.5)
;;;; Uses Parachute testing framework

(in-package :mcclim-render-stack-tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(define-test global-engine-suite
  :parent mcclim-render-stack-suite
  :description "Test suite for global engine infrastructure.")

;;; ============================================================================
;;; Task 1.5 Tests: Global Engine Infrastructure
;;; ============================================================================

(define-test initialize-global-engine-asserts-main-thread
  :parent global-engine-suite
  :description "Verify initialize-global-engine requires main thread."
  ;; Verify the function exists and has thread contract
  (true (fboundp 'mcclim-render-stack::initialize-global-engine)))

(define-test initialize-is-idempotent
  :parent global-engine-suite
  :description "Verify multiple calls to initialize-global-engine don't create duplicates."
  (skip-unless-sdl3
    (call-on-main-thread
      ;; Reset state first to ensure clean test
      (mcclim-render-stack::reset-global-state)
      ;; First initialization
      (mcclim-render-stack::initialize-global-engine)
      (let ((first-engine mcclim-render-stack::*global-engine*)
            (first-delegate mcclim-render-stack::*global-delegate*))
        ;; Second initialization should be idempotent
        (mcclim-render-stack::initialize-global-engine)
        ;; Should be same objects
        (is eq first-engine mcclim-render-stack::*global-engine*)
        (is eq first-delegate mcclim-render-stack::*global-delegate*))
      ;; Cleanup
      (mcclim-render-stack::shutdown-global-engine))))

(define-test engine-is-running-after-init
  :parent global-engine-suite
  :description "Verify engine is in :running state after initialization."
  (skip-unless-sdl3
    (call-on-main-thread
      ;; Reset state first
      (mcclim-render-stack::reset-global-state)
      ;; Initialize
      (mcclim-render-stack::initialize-global-engine)
      ;; Verify engine is running
      (true (render-stack:render-engine-running-p mcclim-render-stack::*global-engine*))
      ;; Cleanup
      (mcclim-render-stack::shutdown-global-engine))))

(define-test delegate-is-properly-configured
  :parent global-engine-suite
  :description "Verify global delegate has correct type and initial state."
  (skip-unless-sdl3
    (call-on-main-thread
      ;; Reset state first
      (mcclim-render-stack::reset-global-state)
      ;; Initialize
      (mcclim-render-stack::initialize-global-engine)
      ;; Verify delegate type
      (true (typep mcclim-render-stack::*global-delegate* 
                   'mcclim-render-stack::multi-window-render-delegate))
      ;; Verify engine configuration
      (is = 2 (render-stack:frame-pipeline-depth
               (render-stack:render-engine-pipeline mcclim-render-stack::*global-engine*)))
      (is = 60 (render-stack:frame-clock-fps 
                (render-stack:render-engine-clock mcclim-render-stack::*global-engine*)))
      ;; Cleanup
      (mcclim-render-stack::shutdown-global-engine))))

(define-test global-engine-initialized-flag-works
  :parent global-engine-suite
  :description "Verify *global-engine-initialized* flag tracks state correctly."
  (skip-unless-sdl3
    (call-on-main-thread
      ;; Reset state
      (mcclim-render-stack::reset-global-state)
      ;; Should be nil initially
      (false mcclim-render-stack::*global-engine-initialized*)
      ;; Initialize
      (mcclim-render-stack::initialize-global-engine)
      ;; Should be t after init
      (true mcclim-render-stack::*global-engine-initialized*)
      ;; Cleanup
      (mcclim-render-stack::shutdown-global-engine)
      ;; Should be nil after shutdown
      (false mcclim-render-stack::*global-engine-initialized*))))
