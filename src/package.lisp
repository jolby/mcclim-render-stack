(defpackage :mcclim-render-stack
  (:use :clim :clim-lisp :clim-backend)
  (:nicknames :clim-render-stack)
  (:local-nicknames (:frs :flutter-render-stack)
                    (:rs-host :render-stack-host)
                    (:rs-sdl3 :render-stack-sdl3)
                    (:rs-internals :render-stack-internals)
                    (:log :org.shirakumo.verbose))
  (:import-from :climi
                #:standard-pointer
                #:pointer-sheet
                #:port-modifier-state
                ;; Event classes
                #:window-destroy-event
                #:window-manager-delete-event
                #:window-configuration-event
                #:window-repaint-event
                #:window-manager-focus-event
                #:window-map-event
                #:window-unmap-event
                #:pointer-enter-event
                #:pointer-exit-event
                #:key-press-event
                #:key-release-event
                #:pointer-button-press-event
                #:pointer-button-release-event
                #:pointer-motion-event
                #:pointer-scroll-event)
  (:import-from :render-stack
                #:render-delegate
                #:make-render-engine
                #:render-engine-start
                #:render-engine-stop
                #:render-engine-pipeline
                #:pipeline-try-consume
                #:render-delegate-begin-frame
                #:render-delegate-end-frame
                #:render-delegate-notify-idle
                #:render-delegate-draw)
   (:export
    ;; Port
    #:render-stack-port

    ;; Runtime
    #:render-stack-runtime
    #:runtime-port
    #:runtime-engine
    #:runtime-impeller-context
    #:runtime-typography-context
    #:runtime-initialized-p
    #:initialize-runtime
    #:initialize-runtime-impeller-context
    #:shutdown-runtime

    ;; Mirror
    #:render-stack-mirror
    #:mirror-sdl-window
    #:mirror-window-id
    #:mirror-gl-context
    #:mirror-port
    #:mirror-sheet
    #:mirror-surface
    #:mirror-width
    #:mirror-height
    #:register-mirror
    #:deregister-mirror
    #:find-mirror-by-window-id
    #:find-sheet-by-window-id
    #:get-or-create-mirror-surface
    #:invalidate-mirror-surface

    ;; Pointer
    #:render-stack-pointer

    ;; Medium
    #:render-stack-medium

    ;; Graft
    #:render-stack-graft

    ;; Frame manager
    #:render-stack-frame-manager
    #:render-stack-frame-mixin

    ;; Server path
    #:initialize-render-stack

    ;; Runner phases
    #:make-clim-event-drain-phase
    #:make-clim-render-phase
    #:make-clim-runner-phases

    ;; Test helpers
    #:make-rgba-color
    #:rgba-color))

(in-package :mcclim-render-stack)

;;; Register the server path
(setf (get :render-stack :port-type) 'render-stack-port)
(setf (get :render-stack :server-path-parser) 'parse-render-stack-server-path)

(defun parse-render-stack-server-path (path)
  "Parse a :render-stack server path."
  (declare (ignore path))
  (list :render-stack))
