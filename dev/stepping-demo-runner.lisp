
(ql:quickload '(:mcclim-render-stack :mcclim-render-stack/examples
                :mcclim-render-stack/test-rig) :force t)

(defpackage :test-rig-user
  (:use :cl)
  (:local-nicknames
   (:frs :flutter-render-stack)
   (:rs-host :render-stack-host)
   (:rs-sdl3 :render-stack-sdl3)
   (:rs-internals :render-stack-internals)
   (:mrc          :mcclim-render-stack)
   (:log :org.shirakumo.verbose)))

(in-package :test-rig-user)


(defvar *demo-thread* NIL)
(defvar *demo-log-level* :warn) ;; :info, :debug ...

(defun run-demo ()
  (setf *demo-thread*
        (bt2:make-thread
         (lambda () (rs-simple-frame:run)) :name "demo")
        (log:repl-level)
        *demo-log-level*))
  

;; Example frame stepping session
#+(or)
(progn

  (run-demo)
  (setf (log:repl-level) *demo-log-level*)

  ;; 3. Enable frame-step (takes effect on the next frame that completes)
  (rs-test-rig:start-frame-step)
  ;; rs-test-rig:*frame-step-mode*  ; => T

  ;; 4. Advance past first-frame reveal sequence (~5-7 frames until window shows)
  (rs-test-rig:advance-frames 8)

  ;; 5. Take a baseline snapshot
  (rs-test-rig:snapshot-frame "/tmp/rs-01-baseline.ppm")

  ;; 6. Advance N more frames (e.g. to let any pending redraws settle)
  (rs-test-rig:advance-frames 3)
  (rs-test-rig:snapshot-frame "/tmp/rs-02-settled.ppm")

  ;; 7. Advance with snapshot in one call (captures last of N frames)
  (rs-test-rig:advance-frames 5 :snapshot-path "/tmp/rs-03-after5.ppm")

  ;; 8. Resume free-running (unblocks the render loop immediately)
  (rs-test-rig:stop-frame-step)


  )

;; Example clicking button
#+(or)
(progn
  ;; Find the window ID (shown in startup log as "win-id=N", or ask)
  (rs-test-rig:get-window-ids)  ; => (1) or similar

  ;; Hover the Hello button first (activates hover state)
  ;; In a 600x450 simple-frame, button bar is ~y=370, Hello button ~x=30
  (rs-test-rig:inject-mouse-move 30 370 :window-id 2)
  (rs-test-rig:advance-frames 2)
  (rs-test-rig:snapshot-frame "/tmp/rs-hover.ppm")

  ;; Click Hello button
  (rs-test-rig:inject-click 30 370 :window-id 2)
  (rs-test-rig:advance-frames 2)
  (rs-test-rig:snapshot-frame "/tmp/rs-after-click.ppm")

  ;; Or step through press and release separately:
  (rs-test-rig:inject-mouse-down 30 370)
  (rs-test-rig:advance-frames 1)
  (rs-test-rig:inject-mouse-up 30 370)
  (rs-test-rig:advance-frames 2)


  )
