;;;; example/multi-window.lisp
;;;; Multi-window test for McCLIM SDL3+Impeller backend
;;;; Launches two top-level application frames to verify event routing.

(in-package :cl-user)

(require :asdf)

;;;; Load the backend system
(asdf:load-system :mcclim-sdl3-impeller-backend)

;;;; Load the simple frame example if not already loaded
(let ((simple-frame-file (asdf:system-relative-pathname 
                          :mcclim-sdl3-impeller-backend 
                          "example/simple-frame.lisp")))
  (load simple-frame-file))

(defpackage :impeller-multi-window
  (:use :clim :clim-lisp)
  (:import-from :impeller-simple-frame #:simple-frame)
  (:export #:run))

(in-package :impeller-multi-window)

(defun run ()
  "Run two simple frames in separate threads using the SDL3+Impeller backend.

   The auto-main-loop will handle starting the event loop on the main thread.
   The first run-frame-top-level will trigger main loop startup, and subsequent
   frames on other threads will work since the loop is already running."
  (format t "~&Launching Multi-Window Demo...~%")
  (setf clim:*default-server-path* '(:sdl3-impeller))

  ;; Launch second window in background thread (will wait for main loop)
  (let ((p2 (bt:make-thread
             (lambda ()
               (sleep 0.5) ;; Wait for first window to start main loop
               (let ((frame (make-application-frame 'simple-frame
                                                    :pretty-name "Window 2 (Right)"
                                                    :width 400 :height 300)))
                 (run-frame-top-level frame)))
             :name "Multi-Window 2")))

    ;; Run first window on this thread (will start main loop)
    (unwind-protect
         (run-frame-top-level (make-application-frame 'simple-frame
                                                      :pretty-name "Window 1 (Left)"
                                                      :width 400 :height 300))
      ;; Wait for second window to finish
      (when (bt:thread-alive-p p2)
        (bt:join-thread p2))))

  (format t "~&Multi-window demo finished.~%"))
