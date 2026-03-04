;;;; harness.lisp -- Tier 2a: REPL-driven frame-step harness
;;;;
;;;; Provides start/stop-frame-step and advance-frames/snapshot-frame.
;;;; Works by parking the main thread's render loop on a semaphore pair
;;;; after each frame, giving the REPL thread exact frame-by-frame control.
;;;;
;;;; Threading model:
;;;;   Main thread  -- runs render-delegate-draw; signals *frame-done-semaphore*
;;;;                   then blocks on *frame-advance-semaphore* after each frame.
;;;;   REPL thread  -- calls advance-frames; signals *frame-advance-semaphore*
;;;;                   then waits on *frame-done-semaphore* for completion.
;;;;
;;;; Event drain timing:
;;;;   While the main thread is parked on *frame-advance-semaphore*, the runner
;;;;   loop is blocked mid-iteration.  SDL events accumulate in SDL's queue.
;;;;   When advance-frames unblocks the main thread, the NEXT runner iteration
;;;;   drains those events FIRST (event-drain phase), then renders -- so
;;;;   inject-mouse-* before advance-frames is correctly ordered.

(in-package :mcclim-render-stack/test-rig)

;;; ============================================================================
;;; Frame-step enable / disable
;;; ============================================================================

(defun start-frame-step ()
  "Enable frame-step mode.  After each complete frame swap the render loop
parks itself waiting for advance-frames or snapshot-frame.
Safe to call from any thread.  Idempotent if already in frame-step mode."
  ;; Create semaphores BEFORE setting the mode flag so the main thread
  ;; never sees mode=T with nil semaphores.
  (setf mcclim-render-stack:*frame-done-semaphore*
        (bt2:make-semaphore :name "rs-frame-done" :count 0)
        mcclim-render-stack:*frame-advance-semaphore*
        (bt2:make-semaphore :name "rs-frame-advance" :count 0))
  (setf mcclim-render-stack:*frame-step-mode* t)
  (values))

(defun stop-frame-step ()
  "Disable frame-step mode.  If the render loop is currently parked, unblocks
it first so it can return to free-running.  Safe to call from any thread."
  ;; Clear mode flag FIRST so the main thread won't re-park after we unblock.
  (setf mcclim-render-stack:*frame-step-mode* nil)
  ;; Unblock the main thread if it is parked.
  (let ((sem mcclim-render-stack:*frame-advance-semaphore*))
    (when sem
      (bt2:signal-semaphore sem)))
  (setf mcclim-render-stack:*frame-done-semaphore* nil
        mcclim-render-stack:*frame-advance-semaphore* nil)
  (values))

;;; ============================================================================
;;; Internal one-frame advance
;;; ============================================================================

(defun %advance-one-frame (&optional (timeout-seconds 5))
  "Signal the main thread to render one frame and wait for it to complete.
Signals an error if the frame does not complete within TIMEOUT-SECONDS."
  (bt2:signal-semaphore mcclim-render-stack:*frame-advance-semaphore*)
  (unless (bt2:wait-on-semaphore mcclim-render-stack:*frame-done-semaphore*
                                 :timeout timeout-seconds)
    (error "Frame-step timeout (~As): render loop did not complete a frame. ~
            Is the demo still running? Check *frame-step-mode*."
           timeout-seconds)))

;;; ============================================================================
;;; Public harness API
;;; ============================================================================

(defun advance-frames (n &key snapshot-path (timeout 5))
  "Advance exactly N frames in frame-step mode, blocking until all complete.

When SNAPSHOT-PATH is provided, the framebuffer is captured during the Nth
frame (before the GL swap) and written as a PPM to that path.

TIMEOUT is per-frame wait time in seconds (default 5).

Signals an error if not in frame-step mode or if any frame times out."
  (unless mcclim-render-stack:*frame-step-mode*
    (error "Not in frame-step mode. Call rs-test-rig:start-frame-step first."))
  (loop for i from 1 to n do
    ;; Set snapshot-path just before the last frame so capture fires there.
    (when (and snapshot-path (= i n))
      (setf *snapshot-path* snapshot-path))
    (%advance-one-frame timeout))
  (values))

(defun snapshot-frame (&optional (path "/tmp/rs-frame.ppm") &key (timeout 5))
  "Advance 1 frame and capture its framebuffer to PATH (PPM format).
Returns PATH on success.  Signals an error if not in frame-step mode or
if the frame times out."
  (unless mcclim-render-stack:*frame-step-mode*
    (error "Not in frame-step mode. Call rs-test-rig:start-frame-step first."))
  (setf *snapshot-path* path)
  (%advance-one-frame timeout)
  path)
