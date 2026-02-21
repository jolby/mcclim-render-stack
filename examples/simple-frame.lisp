;;;; example/simple-frame.lisp

(ql:quickload :mcclim-render-stack :silent t :force t)
(cl:in-package :clim-user)

;;;; This example demonstrates:
;;;; 1. Basic define-application-frame usage
;;;; 2. Simple commands with button gadgets
;;;; 3. Keyboard event handling
;;;; 4. Mouse event handling in application pane
;;;; 5. Window close triggering shutdown

(defvar *click-count* 0)
(defvar *last-key* nil)
(defvar *last-mouse-x* 0)
(defvar *last-mouse-y* 0)

;;;; Forward declarations for frame accessors (they'll be defined later)
(defgeneric frame-click-count (frame))
(defgeneric (setf frame-click-count) (new-value frame))
(defgeneric frame-last-key (frame))
(defgeneric (setf frame-last-key) (new-value frame))
(defgeneric frame-last-pointer-x (frame))
(defgeneric (setf frame-last-pointer-x) (new-value frame))
(defgeneric frame-last-pointer-y (frame))
(defgeneric (setf frame-last-pointer-y) (new-value frame))

;;;; Display functions

(defun display-canvas (frame pane)
  "Draw the main canvas content."
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))
    (draw-rectangle* pane 0 0 w h :filled t :ink +grey90+)
    (draw-text* pane "McCLIM SDL3+Impeller Backend Test"
                (/ w 2) 30 :align-x :center)
    (draw-circle* pane (/ w 2) (/ h 2) 50 :ink +blue+ :filled t)
    (draw-text* pane "Click me!"
                (/ w 2) (/ h 2) :align-x :center :align-y :center
                :ink +white+)
    (let ((x (frame-last-pointer-x frame))
          (y (frame-last-pointer-y frame)))
      (when (and (> x 0) (> y 0))
        (draw-circle* pane x y 5 :ink +red+ :filled t)))
    (finish-output pane)))

(defun display-status (frame pane)
  "Draw the status bar showing current state."
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))
    (draw-rectangle* pane 0 0 w h :filled t :ink +grey80+)
    (draw-line* pane 0 0 w 0 :ink +grey50+)
    (draw-text* pane
                (format nil "Clicks: ~D | Last Key: ~A | Pointer: (~D, ~D)"
                        (frame-click-count frame)
                        (or (frame-last-key frame) "(none)")
                        (frame-last-pointer-x frame)
                        (frame-last-pointer-y frame))
                10 (/ h 2) :align-y :center)
    (finish-output pane)))

;;; Button callbacks

(defun hello-button-callback (gadget)
  "Handle Hello button click - show a message."
  (declare (ignore gadget))
  (format t "~&Hello from McCLIM + SDL3 + Impeller!~%")
  (finish-output))

(defun count-button-callback (gadget)
  "Handle Count button click - increment counter and redisplay."
  (let ((frame (gadget-client gadget)))
    (incf (frame-click-count frame))
    (redisplay-frame-pane frame 'status :force-p t)))

(defun quit-button-callback (gadget)
  "Handle Quit button click - exit the application."
  (let ((frame (gadget-client gadget)))
    (frame-exit frame)))

;;; Application frame definition

(define-application-frame simple-frame ()
  ((click-count :initform 0 :accessor frame-click-count)
   (last-key :initform nil :accessor frame-last-key)
   (last-pointer-x :initform 0 :accessor frame-last-pointer-x)
   (last-pointer-y :initform 0 :accessor frame-last-pointer-y))
  (:panes
   (canvas :application
           :display-function #'display-canvas
           :scroll-bars nil
           :min-width 400
           :min-height 300)
   (hello-button :push-button
                 :label "Hello"
                 :activate-callback #'hello-button-callback)
   (count-button :push-button
                 :label "Count"
                 :activate-callback #'count-button-callback)
   (quit-button :push-button
                :label "Quit"
                :activate-callback #'quit-button-callback)
   (status :application
           :display-function #'display-status
           :scroll-bars nil
           :min-height 60
           :max-height 60))
  (:layouts
   (default
    (vertically ()
      (4/5 canvas)
      (horizontally ()
        hello-button
        count-button
        +fill+
        quit-button)
      status)))
  (:menu-bar nil)
  (:command-table (simple-frame :inherit-from nil)))

;;; Event handling

(defmethod handle-event ((pane application-pane) (event pointer-button-press-event))
  "Handle mouse button press in canvas pane."
  (let ((frame (pane-frame pane))
        (x (pointer-event-x event))
        (y (pointer-event-y event)))
    (setf (frame-last-pointer-x frame) (floor x)
          (frame-last-pointer-y frame) (floor y))
    (incf (frame-click-count frame))
    (redisplay-frame-pane frame 'canvas :force-p t)
    (redisplay-frame-pane frame 'status :force-p t)))

(defmethod handle-event ((pane application-pane) (event pointer-motion-event))
  "Handle mouse motion in canvas pane."
  (let ((frame (pane-frame pane))
        (x (pointer-event-x event))
        (y (pointer-event-y event)))
    (setf (frame-last-pointer-x frame) (floor x)
          (frame-last-pointer-y frame) (floor y))
    (redisplay-frame-pane frame 'status :force-p t)))

(defmethod handle-event ((pane application-pane) (event key-press-event))
  "Handle key press in canvas pane."
  (let ((frame (pane-frame pane))
        (key-name (keyboard-event-key-name event)))
    (setf (frame-last-key frame) key-name)
    (redisplay-frame-pane frame 'status :force-p t)
    (when (member key-name '(:escape :q))
      (frame-exit frame))))

;;; Run function

(defun simple-frame-run (&key (width 600) (height 450))
  "Run the simple frame test application using the SDL3+Impeller backend.
   Uses backend:run-frame-top-level/sdl3 to manage SDL3 main loop on this thread."
  (setf clim:*default-server-path* '(:render-stack))
  (clim:run-frame-top-level (make-application-frame 'simple-frame
                                                    :width width
                                                    :height height)))
