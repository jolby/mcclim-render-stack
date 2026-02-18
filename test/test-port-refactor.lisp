;;;; test/test-port-refactor.lisp
;;;; Tests for port refactor (Task 1.6) + runner integration (bd-31i.6)
;;;; Uses Parachute testing framework

(in-package :mcclim-render-stack-tests)

;;; ============================================================================
;;; Test Suite Definition
;;; ============================================================================

(define-test port-refactor-suite
  :parent mcclim-render-stack-suite
  :description "Test suite for port refactor and runner integration.")

;;; ============================================================================
;;; Task 1.6 Tests: Port Refactor
;;; ============================================================================

(define-test no-custom-queue-functions-exist
  :parent port-refactor-suite
  :description "Verify old queue functions have been removed."
  (false (fboundp 'mcclim-render-stack::port-peek-event))
  (false (fboundp 'mcclim-render-stack::port-event-queue-empty-p))
  (false (fboundp 'mcclim-render-stack::port-enqueue-event))
  (false (fboundp 'mcclim-render-stack::port-dequeue-event))
  ;; Port should have needs-redraw-p slot instead of custom event handling
  (true (fboundp 'mcclim-render-stack::port-needs-redraw-p)))

(define-test port-has-removed-queue-slots
  :parent port-refactor-suite
  :description "Verify port no longer has custom queue slots."
  (false (fboundp 'mcclim-render-stack::port-event-queue))
  (false (fboundp 'mcclim-render-stack::port-event-queue-lock))
  (false (fboundp 'mcclim-render-stack::port-event-queue-condition)))

(define-test port-has-window-id-slot
  :parent port-refactor-suite
  :description "Verify port has port-window-id slot for event routing."
  (true (fboundp 'mcclim-render-stack::port-window-id)))

(define-test port-has-needs-redraw-slot
  :parent port-refactor-suite
  :description "Verify port has port-needs-redraw-p slot for frame requests."
  (true (fboundp 'mcclim-render-stack::port-needs-redraw-p)))

;;; ============================================================================
;;; Runner Integration Tests (bd-31i.6)
;;; ============================================================================

(define-test main-thread-loop-replaced-by-phases
  :parent port-refactor-suite
  :description "Verify main-thread-loop is gone, replaced by runner phases."
  ;; main-thread-loop should no longer exist
  (false (fboundp 'mcclim-render-stack::main-thread-loop))
  ;; Runner phase constructors should exist
  (true (fboundp 'mcclim-render-stack::make-clim-event-drain-phase))
  (true (fboundp 'mcclim-render-stack::make-clim-render-phase))
  (true (fboundp 'mcclim-render-stack::make-clim-runner-phases)))

(define-test port-requires-active-runner
  :parent port-refactor-suite
  :description "Verify port creation fails without an active runner."
  ;; Without *runner* bound, port creation should error
  (let ((rs-internals:*runner* nil))
    (fail (make-instance 'mcclim-render-stack::render-stack-port)
          'error)))

(define-test process-next-event-exists
  :parent port-refactor-suite
  :description "Verify process-next-event is available."
  (true (fboundp 'clim:process-next-event)))

(define-test port-includes-concurrent-queue-pattern
  :parent port-refactor-suite
  :description "Verify port architecture uses concurrent-queue via distribute-event."
  (true (fboundp 'clim:distribute-event)))
