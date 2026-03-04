(defpackage :mcclim-render-stack/test-rig
  (:nicknames :rs-test-rig)
  (:use :cl)
  (:local-nicknames (:log     :org.shirakumo.verbose)
                    (:sdl3-ffi :%sdl3))
  ;; Re-export snapshot and frame-step vars from the main system so REPL users
  ;; can write (setf rs-test-rig:*snapshot-path* "/tmp/f.ppm") conveniently.
  (:import-from :mcclim-render-stack
                #:*snapshot-path*
                #:*snapshot-every-n-frames*
                #:*frame-capture-hook*
                #:*frame-step-mode*)
  (:export
   ;; Snapshot control (same symbols as mcclim-render-stack, re-exported here)
   #:*snapshot-path*
   #:*snapshot-every-n-frames*
   ;; Frame-step mode flag (re-exported for REPL inspection)
   #:*frame-step-mode*
   ;; PPM writer (useful standalone)
   #:write-ppm-file
   ;; Frame-step harness
   #:start-frame-step
   #:stop-frame-step
   #:advance-frames
   #:snapshot-frame
   ;; Event injection
   #:get-window-ids
   #:inject-mouse-move
   #:inject-mouse-down
   #:inject-mouse-up
   #:inject-click))

(in-package :mcclim-render-stack/test-rig)
