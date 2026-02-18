;; Step 1: Load the system
(ql:quickload :mcclim-render-stack)

;; Step 2: Set the backend
(setf clim:*default-server-path* '(:render-stack))

;; Step 3: Define and run a simple test
(in-package :clim-user)

(define-application-frame hello-test ()
  ()
  (:panes
   (canvas :application
           :display-function (lambda (frame pane)
                               (declare (ignore frame))
                               (draw-rectangle* pane 0 0 100 100 :filled t :ink +red+)
                               (finish-output pane))
           :min-width 200
           :min-height 200))
  (:layouts
   (default canvas)))

(run-frame-top-level (make-application-frame 'hello-test :width 400 :height 300))

