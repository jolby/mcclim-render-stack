;;;; examples/min-button.lisp -- minimal push-button repro harness
;;;;
;;;; Reproduces (or rules out) the button-invisible bug in isolation:
;;;;   one application pane + one push-button, no status bar, no pointer tracking.
;;;;
;;;; Diagnostic setup:
;;;;   (rs-internals:setup-dev-logging :level :info)   ; see Q1/Q2 data
;;;;   (rs-internals:setup-dev-logging :level :debug)  ; also see Q3 data
;;;;   (setf mcclim-render-stack::*debug-frame-limit* 3) ; capture 3 frames then exit
;;;;
;;;; Run:
;;;;   (load "examples/min-button.lisp")
;;;;   (rs-min-button:run)

(ql:quickload :mcclim-render-stack :silent t :force t)

(defpackage :rs-min-button
  (:use :clim :clim-lisp)
  (:export #:run))

(in-package :rs-min-button)

(defun display-canvas (frame pane)
  (declare (ignore frame))
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))
    (draw-rectangle* pane 0 0 w h :filled t :ink +white+)
    (draw-text* pane "min-button harness"
                (/ w 2) (/ h 2) :align-x :center :align-y :center)
    (finish-output pane)))

(defun btn-callback (gadget)
  (declare (ignore gadget))
  (format t "~&min-button: activated~%")
  (finish-output))

(define-application-frame min-button-frame ()
  ()
  (:panes
   (canvas :application
           :display-function #'display-canvas
           :scroll-bars nil
           :width 400
           :min-width 100
           :height 300
           :min-height 100)
   (btn :push-button
        :label "Hello"
        :activate-callback #'btn-callback))
  (:layouts
   (default
    (vertically ()
      canvas
      btn)))
  (:menu-bar nil)
  (:command-table (min-button-frame :inherit-from nil)))

(defun run (&key (width 400) (height 360))
  "Run the minimal button harness.

Before calling, optionally set:
  (rs-internals:setup-dev-logging :level :info)
  (setf mcclim-render-stack::*debug-frame-limit* 3)"
  (setf clim:*default-server-path* '(:render-stack))
  (clim:run-frame-top-level
   (make-application-frame 'min-button-frame
                           :width width
                           :height height)))
