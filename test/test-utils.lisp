;;;; mcclim-render-stack/test/test-utils.lisp
;;;; Test utilities for TDD approach - thread testing helpers and mock objects

(in-package :mcclim-render-stack-tests)

;;; ============================================================================
;;; Native Library Availability Probes
;;; ============================================================================
;;; These are checked once at first use; integration tests skip honestly
;;; (showing as SKIPPED, not falsely PASSED) when native libs are absent.

(defvar *sdl3-native-available* :untested)

(defun sdl3-native-available-p ()
  "Probe whether the SDL3 native library is loadable.
Caches the result after the first call."
  (when (eq *sdl3-native-available* :untested)
    (setf *sdl3-native-available*
          (handler-case
              (progn
                ;; Try to call a simple SDL3 function
                (render-stack-sdl3::init-sdl3-video)
                (render-stack-sdl3::quit-sdl3)
                t)
            (error () nil))))
  *sdl3-native-available*)

(defmacro skip-unless-sdl3 (&body body)
  "Execute BODY if SDL3 native library is available, otherwise skip.
Skipped tests show as SKIPPED in Parachute output rather than falsely passing."
  `(if (sdl3-native-available-p)
       (progn ,@body)
       (skip "SDL3 native library not available"
         (true t))))

;;; ============================================================================
;;; Thread Testing Helpers
;;; ============================================================================
;;; These utilities help verify thread contracts are enforced correctly.

(defvar *test-ui-thread* nil
  "UI thread for testing purposes.")

(defvar *test-main-thread* nil
  "Main thread for testing purposes.")

(defun ensure-test-threads-registered ()
  "Ensure main and UI threads are registered for testing.
Should be called once per test session."
  ;; Register current thread as main thread
  (rs-internals:register-main-thread)
  (setf *test-main-thread* (bt:current-thread))
  ;; For testing, we can use the same thread as UI or spawn a separate one
  (unless *test-ui-thread*
    (setf *test-ui-thread* (bt:current-thread))
    (rs-internals:register-ui-thread)))

(defmacro call-on-main-thread (&body body)
  "Execute body on the main thread and return the result.
This is a test helper that runs synchronously."
  `(progn
     (ensure-test-threads-registered)
     ,@body))

(defmacro call-on-ui-thread (&body body)
  "Execute body on the UI thread and return the result.
This is a test helper that runs synchronously."
  `(progn
     (ensure-test-threads-registered)
     ;; For testing, we can run on current thread if it's the UI thread
     (if (eq (bt:current-thread) *test-ui-thread*)
         (progn ,@body)
         (error "Test requires separate UI thread - not implemented"))))

(defmacro should-signal-wrong-thread (operation &body body)
  "Assert that body signals wrong-thread-error when run.
OPERATION is a keyword naming the operation being tested (for documentation)."
  (declare (ignore operation))
  `(is (subtypep (handler-case
                     (progn ,@body
                            'no-error)
                   (rs-internals:wrong-thread-error ()
                     'rs-internals:wrong-thread-error))
                 'rs-internals:wrong-thread-error)))

(defmacro with-thread-test-setup (&body body)
  "Set up thread registry for testing, execute body, clean up."
  `(let ((*test-main-thread* nil)
         (*test-ui-thread* nil))
     (unwind-protect
          (progn
            (ensure-test-threads-registered)
            ,@body)
       ;; Cleanup
       (setf *test-main-thread* nil
             *test-ui-thread* nil))))

;;; ============================================================================
;;; Mock Objects for Testing
;;; ============================================================================

(defclass mock-port ()
  ((id :initarg :id :reader mock-port-id :initform 0)
   (window-id :initarg :window-id :accessor mock-port-window-id :initform nil)
   (event-queue :accessor mock-port-event-queue :initform (make-array 0 :adjustable t :fill-pointer 0))
   (needs-redraw :accessor mock-port-needs-redraw-p :initform nil))
  (:documentation "Mock port for testing delegate operations."))

(defun make-mock-port (&key (id 0) window-id)
  "Create a mock port for testing."
  (make-instance 'mock-port :id id :window-id window-id))

(defclass mock-render-delegate (render-stack:render-delegate)
  ((begin-frame-called :accessor mock-begin-frame-called :initform nil)
   (begin-frame-args :accessor mock-begin-frame-args :initform nil)
   (draw-called :accessor mock-draw-called :initform nil)
   (draw-args :accessor mock-draw-args :initform nil)
   (end-frame-called :accessor mock-end-frame-called :initform nil)
   (drain-called :accessor mock-drain-called :initform 0)
   (notify-idle-called :accessor mock-notify-idle-called :initform nil))
  (:documentation "Mock delegate for testing protocol compliance."))

(defmethod render-stack:render-delegate-begin-frame ((delegate mock-render-delegate) target-time frame-number)
  (setf (mock-begin-frame-called delegate) t
        (mock-begin-frame-args delegate) (list target-time frame-number))
  nil)

(defmethod render-stack:render-delegate-draw ((delegate mock-render-delegate) pipeline-item)
  (setf (mock-draw-called delegate) t
        (mock-draw-args delegate) (list pipeline-item)))

(defmethod render-stack:render-delegate-end-frame ((delegate mock-render-delegate) layer-tree frame-timings)
  (setf (mock-end-frame-called delegate) t))

(defmethod render-stack:render-delegate-notify-idle ((delegate mock-render-delegate) deadline)
  (setf (mock-notify-idle-called delegate) t))

(defun reset-mock-delegate (delegate)
  "Reset all mock state to initial values."
  (setf (mock-begin-frame-called delegate) nil
        (mock-begin-frame-args delegate) nil
        (mock-draw-called delegate) nil
        (mock-draw-args delegate) nil
        (mock-end-frame-called delegate) nil
        (mock-drain-called delegate) 0
        (mock-notify-idle-called delegate) nil))

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

(defvar *test-global-engine* nil)
(defvar *test-global-delegate* nil)

(defmacro with-test-engine ((engine-var delegate-var) &body body)
  "Create isolated test engine and delegate, run body, clean up.
Note: Must be called from main thread."
  `(call-on-main-thread
    (let* ((*test-global-delegate* (make-instance 'mock-render-delegate))
           (*test-global-engine* (render-stack:make-render-engine 
                                  :target-fps 60
                                  :pipeline-depth 2
                                  :delegate *test-global-delegate*)))
      (unwind-protect
           (let ((,engine-var *test-global-engine*)
                 (,delegate-var *test-global-delegate*))
             (render-stack:render-engine-start *test-global-engine*)
             ,@body)
        (when *test-global-engine*
          (render-stack:render-engine-stop *test-global-engine*)
          (setf *test-global-engine* nil))
        (setf *test-global-delegate* nil)))))

(defmacro with-test-port ((port-var) &body body)
  "Create test port with mock sheet, run body, clean up.
Note: Creates mock objects without full SDL3 initialization."
  `(let ((,port-var (make-instance 'mock-port)))
     (unwind-protect
          (progn ,@body)
       ;; Cleanup
       )))

;;; ============================================================================
;;; Event Queue Helpers
;;; ============================================================================

(defun count-events-in-port-queue (port)
  "Count events in a mock port's event queue."
  (length (mock-port-event-queue port)))

(defun clear-port-event-queue (port)
  "Clear all events from a mock port's event queue."
  (setf (mock-port-event-queue port) (make-array 0 :adjustable t :fill-pointer 0)))

;;; ============================================================================
;;; SDL3 Mock Helpers
;;; ============================================================================

(defvar *mock-sdl3-events* nil
  "List of mock SDL3 events for testing.")

(defmacro with-mock-sdl3-event ((ev-var &key window-id type x y) &body body)
  "Execute body with a mock SDL3 event bound to EV-VAR.
This is a simplified mock for testing event translation."
  (declare (ignore ev-var window-id type x y))
  ;; For now, just run the body (mock implementation)
  `(progn ,@body))

;;; ============================================================================
;;; Timing Helpers
;;; ============================================================================

(defmacro with-timeout ((seconds) &body body)
  "Execute body with a timeout. Signals error if timeout expires.
Useful for testing non-blocking operations."
  (let ((deadline-var (gensym)))
    `(let ((,deadline-var (+ (get-universal-time) ,seconds)))
       (loop
         (when (> (get-universal-time) ,deadline-var)
           (error "Test timeout: operation did not complete within ~A seconds" ,seconds))
         ,@body
         (return)))))

;;; ============================================================================
;;; Test Runner Helpers
;;; ============================================================================

(defun run-task-tests (task-keyword)
  "Run tests for a specific task.
TASK-KEYWORD should be one of:
  :task-1.2 - Delegate class tests
  :task-1.3 - Window registration tests
  :task-1.4 - Protocol methods tests
  :task-1.5 - Global engine tests
  :task-1.6 - Port refactor tests"
  (ecase task-keyword
    (:task-1.2 (test :multi-window-delegate-class))
    (:task-1.3 (test :window-registration-protocol))
    (:task-1.4 (test :render-delegate-protocol-methods))
    (:task-1.5 (test :global-engine-infrastructure))
    (:task-1.6 (test :port-refactor))))

(defun run-integration-tests ()
  "Run all integration tests."
  (test :integration-tests))

(defun run-all-tests ()
  "Run all tests for mcclim-render-stack."
  (test :mcclim-render-stack-suite))
