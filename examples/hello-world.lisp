;;;; hello-world.lisp - Hello World example for McCLIM with render-stack
;;;;
;;;; Usage:
;;;;   (load "hello-world.lisp")
;;;;   (clim-user::hello-world-run)
;;;;
;;;; Press ESC or close window to exit.

(ql:quickload :mcclim-render-stack :silent t)

;;;; Load SDL3 native libraries (required before creating port)
(cffi:define-foreign-library
    (sdl3-clawed
     :search-path (asdf:system-relative-pathname :render-stack-sdl3-ffi
                                                 "src/lib/build/desktop/"))
  (:unix "libsdl3.clawed.so"))

(cffi:load-foreign-library 'sdl3-clawed)

;;;; Now define the example in clim-user package
(in-package :clim-user)

;;; Application state
(defvar *hello-world-click-count* 0)

;;; Display function - draws the main content
(defun hello-world-display-canvas (frame pane)
  "Draw the application canvas."
  (declare (ignore frame))
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))
    ;; Background
    (draw-rectangle* pane 0 0 w h :filled t :ink +grey90+)
    ;; Title
    (draw-text* pane "McCLIM + render-stack Demo"
                (/ w 2) 40
                :align-x :center
                :text-style (make-text-style :sans-serif :bold 24))
    ;; Clickable circle
    (draw-circle* pane (/ w 2) (+ h 100) 60 :ink +blue+ :filled t)
    (draw-text* pane "Click me!"
                (/ w 2) (+ h 100)
                :align-x :center :align-y :center
                :text-style (make-text-style :sans-serif :bold 16)
                :ink +white+)
    ;; Click count
    (draw-text* pane (format nil "Clicks: ~D" *hello-world-click-count*)
                (/ w 2) (+ h 180)
                :align-x :center
                :text-style (make-text-style :sans-serif :roman 18))
    (finish-output pane)))

;;; Event handling
(defmethod handle-event ((pane application-pane) (event pointer-button-press-event))
  "Handle mouse click."
  (declare (ignore event))
  (incf *hello-world-click-count*)
  (redisplay-frame-pane (pane-frame pane) 'canvas :force-p t))

(defmethod handle-event ((pane application-pane) (event key-press-event))
  "Handle key press - quit on Escape or Q."
  (let ((key (keyboard-event-key-name event)))
    (when (member key '(:escape :q))
      (frame-exit (pane-frame pane)))))

;;; Application frame definition
;;; Include RENDER-STACK-FRAME-MIXIN for automatic runner bootstrap.
;;; This lets (run-frame-top-level ...) work from both the main thread
;;; and worker threads (e.g., SLIME) without any extra startup ceremony.
(define-application-frame hello-world
    (clim-render-stack:render-stack-frame-mixin)
  ()
  (:panes
   (canvas :application
           :display-function #'hello-world-display-canvas
           :scroll-bars nil
           :min-width 400
           :min-height 300))
  (:layouts
   (default canvas))
  (:menu-bar nil)
  (:command-table (hello-world :inherit-from nil)))

;;; Run function - call this to start the demo
(defun hello-world-run (&key (width 500) (height 400))
  "Run the Hello World demo using the render-stack backend."
  (setf clim:*default-server-path* '(:render-stack))
  (run-frame-top-level (make-application-frame 'hello-world
                                                :width width
                                                :height height)))

(format t "~&Hello World demo loaded. Run (clim-user::hello-world-run) to start.~%")
