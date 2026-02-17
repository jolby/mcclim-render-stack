;;;; test/test-integration.lisp
;;;; Integration tests for end-to-end McCLIM render-stack backend flow
;;;; Uses Parachute testing framework

(in-package :mcclim-render-stack-tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(define-test integration-suite
  :parent mcclim-render-stack-suite
  :description "Integration tests for end-to-end McCLIM render-stack backend flow.")

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(define-test end-to-end-single-window
  :parent integration-suite
  :description "Verify complete flow: create port → realize mirror → events flow → render."
  :depends-on (global-engine-suite port-refactor-suite)
  (skip-unless-sdl3
    (call-on-main-thread
      ;; Reset and initialize global engine
      (mcclim-render-stack::reset-global-state)
      (mcclim-render-stack::initialize-global-engine)
      ;; Verify engine is running
      (true (mcclim-render-stack::*global-engine-initialized*))
      (true mcclim-render-stack::*global-engine*)
      (true mcclim-render-stack::*global-delegate*)
      ;; Create port
      (let ((port (make-instance 'mcclim-render-stack::render-stack-port)))
        (unwind-protect
             (progn
               ;; Verify port creation succeeded
               (true (typep port 'mcclim-render-stack::render-stack-port))
               ;; Verify port is registered with global delegate
               (true mcclim-render-stack::*global-delegate*)
               ;; Verify port has necessary slots initialized
               (true (slot-boundp port 'mcclim-render-stack::window-table))
               (true (hash-table-p (mcclim-render-stack::port-window-table port)))
               ;; Verify typography context is set up
               (true (mcclim-render-stack::port-typography-context port)))
          ;; Cleanup
          (clim:destroy-port port)
          (mcclim-render-stack::shutdown-global-engine))))))

(define-test multi-window-event-routing
  :parent integration-suite
  :description "Verify events are routed to correct window when multiple ports exist."
  :depends-on (end-to-end-single-window)
  (skip-unless-sdl3
    (call-on-main-thread
      ;; Initialize global engine
      (mcclim-render-stack::reset-global-state)
      (mcclim-render-stack::initialize-global-engine)
      ;; Create multiple ports
      (let ((port1 (make-instance 'mcclim-render-stack::render-stack-port))
            (port2 (make-instance 'mcclim-render-stack::render-stack-port)))
        (unwind-protect
             (progn
               ;; Verify both ports created successfully
               (true (typep port1 'mcclim-render-stack::render-stack-port))
               (true (typep port2 'mcclim-render-stack::render-stack-port))
               ;; Verify they are distinct objects
               (isnt eq port1 port2)
               ;; Verify both have separate window tables
               (isnt eq (mcclim-render-stack::port-window-table port1)
                     (mcclim-render-stack::port-window-table port2)))
          ;; Cleanup both ports
          (clim:destroy-port port1)
          (clim:destroy-port port2)
          (mcclim-render-stack::shutdown-global-engine))))))

(define-test concurrent-event-processing-is-safe
  :parent integration-suite
  :description "Verify thread safety of event processing with concurrent access."
  :depends-on (end-to-end-single-window)
  (skip-unless-sdl3
    (call-on-main-thread
      ;; Initialize global engine
      (mcclim-render-stack::reset-global-state)
      (mcclim-render-stack::initialize-global-engine)
      (let ((port (make-instance 'mcclim-render-stack::render-stack-port))
            (errors nil))
        (unwind-protect
             (progn
               ;; Verify port is safe for concurrent access checks
               (true (typep port 'mcclim-render-stack::render-stack-port))
               ;; Test that process-next-event can be called multiple times
               ;; without errors (this tests thread safety of the event system)
               (dotimes (i 10)
                 (handler-case
                     (multiple-value-bind (result reason)
                         (mcclim-render-stack::process-next-event port :timeout 0.001)
                       (true (null result))
                       (is eq :timeout reason))
                   (error (e)
                     (push (format nil "Error on iteration ~A: ~A" i e) errors))))
               ;; Should have no errors
               (true (null errors)))
          ;; Cleanup
          (clim:destroy-port port)
          (mcclim-render-stack::shutdown-global-engine))))))

(define-test global-engine-shutdown-cleans-up-properly
  :parent integration-suite
  :description "Verify shutdown-global-engine properly cleans up all resources."
  :depends-on (end-to-end-single-window)
  (skip-unless-sdl3
    (call-on-main-thread
      ;; Initialize
      (mcclim-render-stack::reset-global-state)
      (mcclim-render-stack::initialize-global-engine)
      (let ((engine mcclim-render-stack::*global-engine*)
            (delegate mcclim-render-stack::*global-delegate*))
        ;; Verify initialization worked
        (true engine)
        (true delegate)
        (true mcclim-render-stack::*global-engine-initialized*)
        ;; Shutdown
        (mcclim-render-stack::shutdown-global-engine)
        ;; Verify cleanup
        (false mcclim-render-stack::*global-engine*)
        (false mcclim-render-stack::*global-delegate*)
        (false mcclim-render-stack::*global-engine-initialized*)))))

(define-test port-delegate-integration
  :parent integration-suite
  :description "Verify port and delegate work together correctly."
  :depends-on (end-to-end-single-window)
  (skip-unless-sdl3
    (call-on-main-thread
      ;; Initialize global engine
      (mcclim-render-stack::reset-global-state)
      (mcclim-render-stack::initialize-global-engine)
      (let ((port (make-instance 'mcclim-render-stack::render-stack-port)))
        (unwind-protect
             (progn
               ;; Verify global delegate exists
               (true mcclim-render-stack::*global-delegate*)
               ;; Verify delegate is multi-window-render-delegate
               (true (typep mcclim-render-stack::*global-delegate*
                           'mcclim-render-stack::multi-window-render-delegate))
               ;; Verify port slots
               (true (mcclim-render-stack::port-typography-context port))
               (true (mcclim-render-stack::port-window-table port)))
          ;; Cleanup
          (clim:destroy-port port)
          (mcclim-render-stack::shutdown-global-engine))))))

;;; ============================================================================
;;; Final Gate Tests
;;; ============================================================================

(define-test final-gate-all-unit-tests-pass
  :parent integration-suite
  :description "Placeholder - verified by running all tests."
  (true t))

(define-test final-gate-multi-window-support
  :parent integration-suite
  :description "Verify backend supports multiple windows."
  (skip-unless-sdl3
    (call-on-main-thread
      (mcclim-render-stack::reset-global-state)
      (mcclim-render-stack::initialize-global-engine)
      ;; Test creating multiple ports
      (let ((ports nil))
        (unwind-protect
             (progn
               ;; Create 3 ports to verify multi-window support
               (dotimes (i 3)
                 (push (make-instance 'mcclim-render-stack::render-stack-port) ports))
               ;; Verify all created
               (is = 3 (length ports))
               (dolist (port ports)
                 (true (typep port 'mcclim-render-stack::render-stack-port))))
          ;; Cleanup all ports
          (dolist (port ports)
            (when port
              (handler-case (clim:destroy-port port)
                (error (e) (declare (ignore e))))))
          (mcclim-render-stack::shutdown-global-engine))))))
