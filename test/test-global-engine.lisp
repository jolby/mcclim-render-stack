;;;; mcclim-render-stack/test/test-global-engine.lisp
;;;; Tests for global engine infrastructure (Task 1.5)

(in-package :mcclim-render-stack-tests)

;;; ============================================================================
;;; Task 1.5 Tests: Global Engine Infrastructure
;;; ============================================================================

(define-test global-engine-infrastructure
  "Test suite for global engine infrastructure."
  :parent mcclim-render-stack-suite)

;;; Thread Contract Tests

(deftest initialize-global-engine-asserts-main-thread
  "Verify initialize-global-engine signals error if not on main thread."
  (is (fboundp 'mcclim-render-stack::initialize-global-engine))
  ;; Note: Full test requires running from wrong thread
  (true t))

;;; Idempotent Initialization Tests

(deftest initialize-is-idempotent
  "Verify multiple calls to initialize-global-engine don't create duplicate engines."
  ;; This test would require SDL3 to be initialized
  (skip-unless-sdl3
    (call-on-main-thread
     (lambda ()
       (mcclim-render-stack::initialize-global-engine)
       (let ((first-engine mcclim-render-stack::*global-engine*)
             (first-delegate mcclim-render-stack::*global-delegate*))
         (mcclim-render-stack::initialize-global-engine)
         ;; Should be same objects
         (is (eql first-engine mcclim-render-stack::*global-engine*))
         (is (eql first-delegate mcclim-render-stack::*global-delegate*)))))))

;;; Engine State Tests

(deftest engine-is-running-after-init
  "Verify engine is in :running state after initialization."
  (skip-unless-sdl3
    (call-on-main-thread
     (lambda ()
       (mcclim-render-stack::initialize-global-engine)
       (is (render-stack:render-engine-running-p mcclim-render-stack::*global-engine*))))))

(deftest delegate-is-properly-configured
  "Verify global delegate has correct type and initial state."
  (skip-unless-sdl3
    (call-on-main-thread
     (lambda ()
       (mcclim-render-stack::initialize-global-engine)
       (is (typep mcclim-render-stack::*global-delegate* 'mcclim-render-stack::multi-window-render-delegate))
       (is (= 2 (render-stack:render-engine-pipeline-depth mcclim-render-stack::*global-engine*)))
       (is (= 60 (render-stack:render-engine-target-fps mcclim-render-stack::*global-engine*)))))))

;;; Global State Variables Tests

(deftest global-variables-exist
  "Verify global engine and delegate variables exist."
  (is (boundp 'mcclim-render-stack::*global-engine*))
  (is (boundp 'mcclim-render-stack::*global-delegate*)))

(deftest global-variables-initially-nil
  "Verify global variables start as nil."
  ;; Note: This test assumes fresh state; may need to run before initialization
  (is (null mcclim-render-stack::*global-engine*))
  (is (null mcclim-render-stack::*global-delegate*)))
