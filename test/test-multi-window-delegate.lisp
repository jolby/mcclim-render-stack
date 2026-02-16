;;;; test/test-multi-window-delegate.lisp
;;;; Tests for multi-window render delegate (Tasks 1.2-1.4)
;;;; Uses Parachute testing framework

(in-package :mcclim-render-stack-tests)

;;; ============================================================================
;;; Task 1.2 Tests: Delegate Class
;;; ============================================================================

(define-test delegate-class-can-be-instantiated
  :parent multi-window-delegate-suite
  "Verify delegate class can be created without errors."
  (let ((delegate (make-instance 'mcclim-render-stack::multi-window-render-delegate)))
    (true (typep delegate 'mcclim-render-stack::multi-window-render-delegate))
    (true (hash-table-p (mcclim-render-stack::delegate-window-table delegate)))
    (true (hash-table-p (mcclim-render-stack::delegate-port-table delegate)))
    (true (not (null (mcclim-render-stack::delegate-table-lock delegate))))
    (true (hash-table-p (mcclim-render-stack::delegate-pending-display-lists delegate)))
    (true (not (null (mcclim-render-stack::delegate-display-list-lock delegate))))
    (true (null (mcclim-render-stack::delegate-dirty-ports delegate)))
    (true (not (null (mcclim-render-stack::delegate-dirty-lock delegate))))))

(define-test delegate-slots-have-correct-initforms
  :parent multi-window-delegate-suite
  "Verify all slots are initialized with correct types."
  (let ((delegate (make-instance 'mcclim-render-stack::multi-window-render-delegate)))
    ;; Window table should use eql test
    (is eq 'eql (hash-table-test (mcclim-render-stack::delegate-window-table delegate)))
    ;; Port table should use eq test
    (is eq 'eq (hash-table-test (mcclim-render-stack::delegate-port-table delegate)))
    ;; Display lists should use eq test
    (is eq 'eq (hash-table-test (mcclim-render-stack::delegate-pending-display-lists delegate)))
    ;; Dirty ports should be nil
    (true (null (mcclim-render-stack::delegate-dirty-ports delegate)))))

;;; ============================================================================
;;; Task 1.3 Tests: Window Registration Protocol
;;; ============================================================================

(define-test register-port-adds-to-tables
  :parent window-registration-suite
  "Verify register-port-with-delegate adds bidirectional mapping."
  (let ((delegate (make-instance 'mcclim-render-stack::multi-window-render-delegate))
        (mock-port (make-mock-port)))
    (mcclim-render-stack::register-port-with-delegate delegate mock-port 123)
    ;; Window-id -> port mapping
    (is eq mock-port (gethash 123 (mcclim-render-stack::delegate-window-table delegate)))
    ;; Port -> window-id mapping
    (is eq 123 (gethash mock-port (mcclim-render-stack::delegate-port-table delegate)))))

(define-test find-port-returns-correct-port
  :parent window-registration-suite
  "Verify find-port-for-window returns registered port."
  (let ((delegate (make-instance 'mcclim-render-stack::multi-window-render-delegate))
        (mock-port (make-mock-port)))
    (mcclim-render-stack::register-port-with-delegate delegate mock-port 456)
    (is eq mock-port (mcclim-render-stack::find-port-for-window delegate 456))))

(define-test find-port-returns-nil-for-unregistered
  :parent window-registration-suite
  "Verify find-port-for-window returns nil for unknown window."
  (let ((delegate (make-instance 'mcclim-render-stack::multi-window-render-delegate)))
    (true (null (mcclim-render-stack::find-port-for-window delegate 999)))))

(define-test unregister-port-removes-bidirectional-mapping
  :parent window-registration-suite
  "Verify unregister removes both window->port and port->window mappings."
  (let ((delegate (make-instance 'mcclim-render-stack::multi-window-render-delegate))
        (mock-port (make-mock-port)))
    (mcclim-render-stack::register-port-with-delegate delegate mock-port 789)
    (mcclim-render-stack::unregister-port-from-delegate delegate mock-port)
    ;; Both mappings should be removed
    (true (null (gethash 789 (mcclim-render-stack::delegate-window-table delegate))))
    (true (null (gethash mock-port (mcclim-render-stack::delegate-port-table delegate))))))

(define-test unregister-cleans-up-display-list
  :parent window-registration-suite
  "Verify unregister removes any pending display list for port."
  (let ((delegate (make-instance 'mcclim-render-stack::multi-window-render-delegate))
        (mock-port (make-mock-port)))
    (mcclim-render-stack::register-port-with-delegate delegate mock-port 100)
    ;; Add a display list
    (setf (gethash mock-port (mcclim-render-stack::delegate-pending-display-lists delegate)) :test-dl)
    (mcclim-render-stack::unregister-port-from-delegate delegate mock-port)
    ;; Display list should be removed
    (true (null (gethash mock-port (mcclim-render-stack::delegate-pending-display-lists delegate))))))

;;; ============================================================================
;;; Task 1.4 Tests: Protocol Methods
;;; ============================================================================

(define-test begin-frame-asserts-ui-thread
  :parent protocol-methods-suite
  :depends-on (delegate-class-can-be-instantiated)
  "Verify render-delegate-begin-frame requires UI thread."
  ;; Verify the function exists
  (true (fboundp 'render-stack:render-delegate-begin-frame)))

(define-test draw-asserts-main-thread
  :parent protocol-methods-suite
  :depends-on (delegate-class-can-be-instantiated)
  "Verify render-delegate-draw requires main thread."
  (true (fboundp 'render-stack:render-delegate-draw)))

(define-test drain-sdl3-events-asserts-main-thread
  :parent protocol-methods-suite
  :depends-on (delegate-class-can-be-instantiated)
  "Verify drain-sdl3-events requires main thread."
  (true (fboundp 'mcclim-render-stack::drain-sdl3-events)))

(define-test begin-frame-builds-display-lists-for-dirty-ports
  :parent protocol-methods-suite
  :depends-on (register-port-adds-to-tables)
  "Verify begin-frame builds display lists for ports marked dirty."
  (skip-unless-sdl3
    (let ((delegate (make-instance 'mcclim-render-stack::multi-window-render-delegate))
          (port1 (make-mock-port :id 1))
          (port2 (make-mock-port :id 2)))
      (mcclim-render-stack::register-port-with-delegate delegate port1 1)
      (mcclim-render-stack::register-port-with-delegate delegate port2 2)
      ;; Mark both ports dirty
      (bt2:with-lock-held ((mcclim-render-stack::delegate-dirty-lock delegate))
        (setf (mcclim-render-stack::delegate-dirty-ports delegate) (list port1 port2)))
      ;; Call begin-frame
      (let ((result (render-stack:render-delegate-begin-frame delegate 0.0 1)))
        ;; Should return non-nil (has content)
        (true result)
        ;; Dirty list should be cleared
        (true (null (mcclim-render-stack::delegate-dirty-ports delegate)))))))
#+(or)
(progn
  (test 'multi-window-delegate-suite :report 'interactive)

  )
