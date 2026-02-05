(in-package :mcclim-render-stack-tests)

(in-suite :mcclim-render-stack)

;;; Basic tests - these will be expanded as implementation progresses

(test server-path-registered
  "Test that the :render-stack server path is registered."
  (is (not (null (get :render-stack :port-type)))))

(test port-creation
  "Test basic port creation."
  (let ((port (make-instance 'render-stack-port)))
    (is (typep port 'render-stack-port))
    (is (null (render-stack-port-pipeline port)))))

(test medium-creation
  "Test basic medium creation."
  (let ((medium (make-instance 'render-stack-medium)))
    (is (typep medium 'render-stack-medium))))

(test graft-dimensions
  "Test graft dimension queries."
  (let ((graft (make-instance 'render-stack-graft)))
    (is (numberp (graft-width graft)))
    (is (numberp (graft-height graft)))
    (is (plusp (graft-width graft)))
    (is (plusp (graft-height graft)))))
