Inject UI events into the running demo, advance frames, and capture the result.

**Usage:** `/render-step [event sequence description]`

**Examples:**
- `/render-step hover at (30, 370) then snapshot`
- `/render-step click Hello button at (30, 370) and check for visual change`
- `/render-step move mouse to (100, 200) advance 3 frames snapshot`

---

## Prerequisites

The eval server and demo must be running. Frame-step must be active.
Start with:
```
scripts/rs-eval.sh '(rs-test-rig:start-frame-step)'
scripts/rs-eval.sh '(rs-test-rig:advance-frames 10)'  ; let window appear
```

Or use `/render-snapshot` first (which handles frame-step internally via capture-frame).

---

## Available eval-server commands

Run each via `scripts/rs-eval.sh 'EXPR'`:

```lisp
; Advance N frames (render thread must be in frame-step mode)
(rs-test-rig:advance-frames N)

; Advance N frames and capture last one
(rs-test-rig:advance-frames N :snapshot-path "/tmp/rs-step.ppm")

; Snapshot current frame (1 advance + capture)
(rs-test-rig:snapshot-frame "/tmp/rs-step.ppm")

; Inject mouse events (inject BEFORE advance-frames to process in next iteration)
(rs-test-rig:inject-mouse-move X Y)
(rs-test-rig:inject-mouse-down X Y)          ; :button 1/2/3
(rs-test-rig:inject-mouse-up X Y)
(rs-test-rig:inject-click X Y)               ; down + up

; Get window IDs (if needed)
(rs-test-rig:get-window-ids)
```

---

## Steps

Based on the event sequence description, construct and run the appropriate sequence of `scripts/rs-eval.sh` calls. Then:

1. Convert the PPM: `convert /tmp/rs-step.ppm /tmp/rs-step.png` (or `magick`)
2. Read `/tmp/rs-step.png` using the Read tool
3. Describe what changed compared to the baseline (if known), or what is currently rendered
4. Note any rendering anomalies

**Event ordering rule:** Always inject events BEFORE the `advance-frames` call that should process them. The runner drains SDL events at the START of each iteration, then renders.
