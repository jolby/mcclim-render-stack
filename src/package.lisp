(defpackage :mcclim-render-stack
  (:use :clim :clim-lisp :clim-backend)
  (:nicknames :clim-render-stack)
  (:export
   ;; Port
   #:render-stack-port
   #:render-stack-port-pipeline

   ;; Medium
   #:render-stack-medium

   ;; Graft
   #:render-stack-graft

   ;; Frame manager
   #:render-stack-frame-manager

   ;; Server path
   #:initialize-render-stack))

(in-package :mcclim-render-stack)

;;; Register the server path
(setf (get :render-stack :port-type) 'render-stack-port)
(setf (get :render-stack :server-path-parser) 'parse-render-stack-server-path)

(defun parse-render-stack-server-path (path)
  "Parse a :render-stack server path."
  (declare (ignore path))
  (list :render-stack))
