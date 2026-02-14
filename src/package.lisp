(defpackage :mcclim-render-stack
  (:use :clim :clim-lisp :clim-backend)
  (:nicknames :clim-render-stack)
  (:local-nicknames (:frs :flutter-render-stack)
                    (:rs-host :render-stack-host)
                    (:rs-sdl3 :render-stack-sdl3)
                    (:rs-internals :render-stack-internals)
                    (:bt2 :bordeaux-threads))
  (:import-from :render-stack
                #:render-delegate
                #:make-render-engine
                #:render-engine-start
                #:render-engine-stop
                #:render-delegate-begin-frame
                #:render-delegate-end-frame
                #:render-delegate-notify-idle
                #:render-delegate-draw)
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

   ;; Render delegate
   #:clim-render-delegate

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
