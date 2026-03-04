# Test Rig Usage Guide for Developers


  Now I understand both options well. Let me think through the recommendation:

  Option A: Use simple-frame
  - Pros:
    - It's the actual broken demo - using the rig to debug bd-aei2 is the primary goal
    - No new code to write for the subject under test
    - The push-button text missing + hover blackens bar bug is directly observable
  - Cons:
    - The behavior is broken/non-deterministic, so it's harder to know what "correct" looks like
    - Push buttons have complex gadget paint logic that may involve lots of frames before settling
    - Mouse coordinates for the buttons are not known in advance (they're layout-dependent)
    - The status bar constantly triggers redraws on any mouse movement, and running the demo blocks the thread entirely

  Option B: Create a simple test widget
  - Pros:
    - Known, simple behavior - solid color rectangles, predictable coordinates
    - No layout surprises - designed for pixel-level testing
    - The test-rig plan already specifies this (Tier 2c)
  - Cons:
    - More code to write
    - Doesn't directly address the broken bug yet

  For Tier 1, simple-frame is the right choice because I just need to answer whether the button text shows up in the GPU output, and I don't need deterministic frame
  control yet—just on-demand snapshots of the actual thing I'm debugging. The test widget can wait until Tier 2c when frame-stepping is working.

  Now I need to think through the REPL instructions in detail. Someone would need to know the startup sequence—which make target to use, how to load mcclim-render-stack and
   the test-rig, then run the demo in a background thread since it blocks. I should cover setting up *default-server-path*, triggering a snapshot, and where to find the
  output.

  The tricky part is the timing: snapshots happen on the next rendered frame, not immediately, so I need to explain waiting for the window to appear first. I should also
  mention *snapshot-every-n-frames* for watching state changes and how to view the PPM file with whatever image viewer is available. Let me check what the Makefile provides
   for starting the REPL.

∴ Thinking…

  OK, now I have a complete picture. Let me think about the recommendation.

  Recommendation: Use simple-frame for Tier 1.

  Reasons:
  1. It's the subject of the active bug bd-aei2 - the snapshot will directly answer "is button text present in Impeller's output?"
  2. No new code needed for the subject
  3. Tier 1 doesn't require deterministic frame control - just a snapshot on demand

  For Tier 2+ (frame-step), a dedicated test widget will be better because:
  - simple-frame's status bar redraws on every mouse-motion (lots of dirty frames), making deterministic frame counting harder

  - Button coordinates vary based on layout and HiDPI scaling, so the test widget's fixed coordinate approach is more reliable for pixel-level assertions

  Now for the REPL setup, there are some practical constraints to work around. The Makefile's batch flags like --non-interactive and --disable-debugger need to be dropped
  for an interactive session. Since rs-simple-frame:run blocks until the frame exits, I'll need to spawn it in a background thread to keep the REPL responsive.

  The render stack runs on the main thread via trivial-main-thread, so from the REPL thread I can set variables that the main thread picks up. I need to load both the main
  system and test-rig before starting the demo, and the snapshot capture is asynchronous—setting *snapshot-path* gets picked up on the next render loop iteration, which
  happens within ~16ms at 60fps.

  Let me look at dev/simple-app-1.lisp to see if that's a good starting point for the dev REPL.

∴ Thinking…

  Now I have everything I need. Let me write the recommendation and the detailed REPL instructions.

  Recommendation: Use simple-frame for Tier 1.

  The key insight is that simple-frame IS the broken thing we're trying to debug. The snapshot will directly answer whether Impeller is putting the button text in the
  framebuffer or not.

  The test-widget (Tier 2c) is better suited for Tier 2+ where we need frame-determinism and known pixel coordinates for assertions.

  For the REPL instructions, I need to be careful about:
  1. The Makefile's --non-interactive flag - they need to drop it for an interactive REPL

  2. The backend runs render-engine which uses trivial-main-thread:call-in-main-thread internally, but that's inside the CLIM run-frame-top-level thread while the REPL runs
   separately
  3. Since rs-simple-frame:run blocks until the frame exits, I'll need to spawn it in a separate thread using bt2:make-thread
  4. The snapshot variables live in the mcclim-render-stack package and are accessible as rs-test-rig:*snapshot-path* after loading test-rig

  Let me check the simple-frame.lisp run function to confirm the threading approach...

  The run-frame-top-level call blocks, so I definitely need to run this in a background thread. The tricky part is that the render engine requires the main thread via
  trivial-main-thread, but when I call run-frame-top-level from a background thread, the CLIM backend's port initialization will handle submitting the render engine setup
  to the main thread anyway. The REPL thread is the main thread here, so this should work out. interactive SBCL (when started normally), unless we're using make deps-core
  which might change things.

  Hmm actually, in an interactive SBCL session:
  - The main thread IS the initial thread where the REPL is running
  - trivial-main-thread on SBCL Linux dispatches to the initial thread

  So if we're running the REPL on the main thread and call bt2:make-thread to run simple-frame, then the render engine initialization (via submit-to-main-thread) should
  correctly dispatch back to the main REPL thread. But the REPL is also using that thread...

  Actually, looking at the render-stack-internals code (from memory), submit-to-main-thread uses trivial-main-thread:call-in-main-thread which on Linux/SBCL queues tasks to
   be processed on the initial thread. In an interactive REPL, that initial thread is running the event loop, so trivial-main-thread would handle the queued work between
  REPL interactions—though this depends on how the implementation actually processes those items while the REPL is blocking on input.

  I think the practical approach here is simpler: load the test-rig and simple-frame examples, spawn simple-frame in a background thread with bt2:make-thread, keep the REPL
   available on the main thread, and set snapshot variables directly from the REPL. That avoids the threading complexity altogether. about the main-thread requirement...

  Actually, looking at runner-phases.lisp and the port initialization - the render engine starts a loop that calls render-delegate-draw on the main thread via
  trivial-main-thread. In an interactive SBCL session, trivial-main-thread should work fine because it queues work for the initial thread.

  But if the REPL IS on the initial thread and is blocked waiting for user input, then trivial-main-thread tasks won't get executed until after the current REPL form
  returns. That would be a deadlock if simple-frame's initialization blocks waiting for the main thread.

  The actual flow in the render-stack backend is: run-frame-top-level spawns a background thread that calls make-application-frame, which triggers port initialization. The
  port's :after method submits runtime initialization to the main thread via trivial-main-thread:call-in-main-thread, which on Linux/SBCL uses thread interrupts to execute
  on the initial thread. This is genuinely problematic in an interactive REPL scenario, but the non-interactive make run-demo target should work fine. I could either
  document this limitation or explore a simpler approach for the Tier 1 snap setup. shots in the context of the existing demo setup:
  1. Start the demo with make demo-simple-frame but in interactive mode (drop --disable-debugger --non-interactive)
  2. Add SBCL's --eval to load the test-rig BEFORE starting the demo
  3. The demo window appears, and from a separate terminal (or via slynk/swank if set up) you can interact

  But actually the user said "REPL session" - they probably have swank or sly running and connect to a running SBCL image. Let me assume they start an SBCL image with the
  deps loaded, connect, and then work interactively.

  The simplest REPL workflow would be to start SBCL interactively without the non-interactive flags, load quicklisp and set up the ASDF registry, then load the test-rig
  which brings in mcclim-render-stack as a dependency, load the simple-frame example, start it in a background thread, and then manipulate the snapshot variable from the
  REPL while the render engine picks up changes on each frame and writes to /tmp/frame.ppm.

  Now I'm thinking through the trivial-main-thread concern - the render-stack wraps all Impeller calls with float trap masking and uses trivial-main-thread to dispatch work
   back to the main thread, which means I need to understand how the runner phases coordinate with this threading model.

  Rather than diving deep into the threading details, I should just lay out the practical approach and note the constraints so the user can decide if it's viable for their
  setup.

∴ Thinking…

  Now I have a full picture. Let me check the port.lisp to understand how the main-thread-runner is started.

∴ Thinking…

  Now I understand the full picture:

  Case B (SLIME/Emacs, async evaluation) is the REPL scenario:
  - The REPL runs on a worker thread (not the OS main thread)
  - run-frame-top-level :around detects this is a worker thread
  - It starts the runner on the OS main thread via run-runner-on-main-thread
  - Then calls call-next-method on the REPL thread (which runs the event loop)
  - run-frame-top-level blocks the REPL thread until the application exits

  So if called directly from the REPL thread, rs-simple-frame:run will BLOCK the REPL thread until the app closes.

  To keep the REPL responsive, I need to spawn rs-simple-frame:run in a background thread using bt2:make-thread. I think I have enough context now to write the answer. For
  the immediate goal of determining whether button text appears in Impeller's output or if it's a CLIM rendering issue, simple-frame is the right choice for the first
  snapshot session, with the test-widget serving as the ideal subject for automated pixel assertions later.

  The key points for the REPL instructions are that calling run-frame-top-level from SLIME blocks the REPL thread since it runs the event loop on the calling thread, so
  backgrounding the call is essential. The mcclim-render-stack/test-rig module needs to load before rendering starts, and *snapshot-path* gets read on the next render
  cycle.

  The render loop continuously produces frames at a fallback rate of 30fps when idle, so snapshots will be captured even without user interaction. For the simple-frame
  demo, the button bar sits around y=360 in a 600x450 window, which I should document so users know where to click. Standard image viewers like feh or eog can display the
  PPM output.

  Once I capture that first snapshot, I can diagnose whether the text renders correctly—if it does, the CLIM model has a bug; if it's missing, the issue is in Impeller or
  the compositor. That'll tell me exactly where to focus the fix.

● Now I have the full picture. Let me give the recommendation and detailed instructions.

  ---
  Recommendation: use simple-frame

  The test-widget (Tier 2c) is for automated pixel assertions where you need known coordinates and deterministic frame stepping. For the immediate Tier 1 goal — "is button
  text present in Impeller's framebuffer output?" — simple-frame is the right subject because:

  - It's the actual broken thing (bd-aei2)
  - One snapshot answers the key diagnostic question immediately
  - No new code to write or load

  ---
  Detailed REPL Session Instructions

  1. Start an interactive SBCL

  The Makefile's demo-simple-frame target uses --disable-debugger --non-interactive, which is wrong for REPL use. Start SBCL yourself:

  cd repos/mcclim-render-stack

  sbcl --dynamic-space-size 8096 \
       --eval '(require :asdf)' \
       --eval '(pushnew #p"./" asdf:*central-registry*)' \
       --eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))'

  Or, if you have an existing REPL / SLIME / SLY session, just use that — the bootstrap handles worker-thread invocation (see Case B below).

  ---
  2. Load systems

  ;; Load the backend + the test rig (test-rig depends on mcclim-render-stack,
  ;; so one call is enough)
  (ql:quickload :mcclim-render-stack/test-rig)

  ;; Verify the hook was installed:
  mcclim-render-stack:*frame-capture-hook*   ; => #<CLOSURE ...>, not NIL

  ;; Load the simple-frame example
  (load "examples/simple-frame.lisp")

  ---
  3. Start the demo in a background thread

  run-frame-top-level blocks the calling thread. From a REPL worker thread (SLIME/SLY), the run-frame-top-level :around method handles Case B — it starts the main-thread
  runner on the OS main thread, then runs the McCLIM event loop on the calling thread. So you must call run from a background thread to keep the REPL available:

  (defvar *demo-thread*
    (bt2:make-thread
      (lambda ()
        (setf clim:*default-server-path* '(:render-stack))
        (rs-simple-frame:run))
      :name "simple-frame-demo"))

  The window appears within a few seconds. Wait for it to be fully visible before snapping (the first-frame reveal sequence must complete).

  ---
  4. Capture a snapshot

  Setting *snapshot-path* is picked up on the next rendered frame (within ~16-33ms; the fallback renderer runs at ~30fps even when nothing is dirty):

  ;; Capture whatever is currently on screen
  (setf rs-test-rig:*snapshot-path* "/tmp/rs-initial.ppm")

  ;; Wait a moment, then confirm the var was cleared (capture happened):
  rs-test-rig:*snapshot-path*   ; => NIL  means capture completed
                                ; => still "/tmp/rs-initial.ppm"  means no frame rendered yet

  The :test-rig log line will also confirm: Captured 600x450 framebuffer to /tmp/rs-initial.ppm.

  View it:
  feh /tmp/rs-initial.ppm
  # or: eog /tmp/rs-initial.ppm
  # or: display /tmp/rs-initial.ppm   (ImageMagick)
  # or: xdg-open /tmp/rs-initial.ppm

  ---
  5. The diagnostic questions to answer

  In a 600×450 window the layout is approximately:

  y=0   ┌─────────────────────────────────┐
        │  canvas  (0..359, 4/5 of 450)   │
  y=360 ├─[Hello]─[Count]──────────[Quit]─┤  ← button bar
  y=390 ├─────────────────────────────────┤
        │  status bar  (60px)             │
  y=450 └─────────────────────────────────┘

  Pixels to inspect in the snapshot:
  - Button label area (~x=20, y=375): should show dark text on a button-colored background. If it's all one flat color (no text pixels), the bug is in Impeller's output.
  - Button bar background (~x=300, y=375): the "hover blackens bar" bug should be visible at rest vs after moving the mouse over the bar.

  ---
  6. Capture multiple states

  ;; State 1: initial (no mouse interaction)
  (setf rs-test-rig:*snapshot-path* "/tmp/rs-01-initial.ppm")
  ;; (wait for NIL confirmation)

  ;; Move the mouse over the Hello button in the window, then:
  (setf rs-test-rig:*snapshot-path* "/tmp/rs-02-hover.ppm")
  ;; (wait for NIL confirmation)

  ;; Compare:
  ;; diff /tmp/rs-01-initial.ppm /tmp/rs-02-hover.ppm  (binary, won't be useful)
  ;; Use an image viewer side-by-side instead

  Or use the auto-capture mode to watch state evolve without manual intervention:

  ;; Capture every 30 frames → /tmp/frame-000030.ppm, /tmp/frame-000060.ppm, ...
  (setf rs-test-rig:*snapshot-every-n-frames* 30)
  ;; move the mouse, hover, click
  (setf rs-test-rig:*snapshot-every-n-frames* nil)  ; stop

  ---
  7. Exit the demo

  ;; Click the Quit button in the window, or press Escape/Q while the canvas has focus
  ;; The demo thread exits naturally and *demo-thread* is done.

  ;; Or if you need to force it:
  (bt2:destroy-thread *demo-thread*)  ; last resort only

  ---
  Key caveats

  - No *snapshot-path* if test-rig not loaded: The hook is installed at ppm.lisp load time. If you forget to load mcclim-render-stack/test-rig, the hook is nil and setting
  *snapshot-path* is harmless but nothing happens.
  - GL context requirement: glReadPixels reads from the GL context on the main thread. This is correct — the hook runs inside render-delegate-draw on the main thread. Never
   call it manually from the REPL thread.
  - Snapshot happens on next frame, not immediately: The render loop may be paused if no dirty panes and the pipeline is idle. If the window is stuck, wiggling the mouse or
   clicking will trigger a frame.
  - PPM coordinate origin: (0,0) is top-left in the PPM file (rows are flipped from GL's bottom-up output by write-ppm-file), so pixel coordinates in the PPM match logical
  window coordinates directly.

# The stepper
● All balanced. Here's everything you need:

  ---
  Silencing the log flood

  After the window appears (runtime initialized, which calls setup-dev-logging defaulting to :debug), override it from the REPL:

  ;; Option A: silence REPL, keep nothing (good for clean frame-step sessions)
  (setf (log:repl-level) :warn)

  ;; Option B: silence REPL, redirect debug output to a file you can tail
  (rs-internals:setup-production-logging :level :warn
                                         :log-file #p"/tmp/rs-debug.log")
  ;; then in a terminal: tail -f /tmp/rs-debug.log

  ---
  Frame-step REPL session

  ;; 1. Load systems and start demo as before
  (ql:quickload :mcclim-render-stack/test-rig)
  (load "examples/simple-frame.lisp")
  (defvar *demo* (bt2:make-thread (lambda () (rs-simple-frame:run)) :name "demo"))

  ;; 2. Silence logs once window is up
  (setf (log:repl-level) :warn)

  ;; 3. Enable frame-step (takes effect on the next frame that completes)
  (rs-test-rig:start-frame-step)
  rs-test-rig:*frame-step-mode*  ; => T

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

  Key behaviours:
  - advance-frames blocks the calling thread until all N frames complete — no polling needed.
  - While parked, the runner loop is fully stalled, so the 30fps fallback cannot sneak in extra frames.
  - advance-frames times out (default 5s/frame) and signals a Lisp error if the runtime crashes, so the REPL won't hang forever.
  - stop-frame-step safely unblocks a parked render loop before clearing the semaphores.
