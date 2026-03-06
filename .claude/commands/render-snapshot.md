Capture a rendered frame from the running mcclim-render-stack demo and analyze it visually.

**Usage:** `/render-snapshot [what to look for]`

**Example:** `/render-snapshot check that the Hello button text is visible`

---

## Prerequisites

The demo must be running with the eval server active. If not:
1. Load system in REPL: `(ql:quickload '(:mcclim-render-stack/examples :mcclim-render-stack/test-rig))`
2. Start demo: `(rs-simple-frame:run)`
3. Start eval server: `(rs-test-rig:start-repl-server)`

---

## Steps

1. **Check server is reachable:**
   ```
   scripts/rs-eval.sh '(identity :ping)'
   ```
   If this fails (exit non-zero or timeout), tell the user the server is not running and stop.

2. **Capture a frame:**
   ```
   scripts/rs-capture.sh 15 /tmp/rs-snapshot.ppm
   ```
   This prints the PNG path (e.g., `/tmp/rs-snapshot.png`) on success.
   If it fails, report the error and stop.

3. **Read the image** using the Read tool on the PNG path printed in step 2.

4. **Analyze what you see:**
   - Describe the overall window layout (background, panes, buttons, text)
   - Note the color and appearance of each visible element
   - Identify anything that looks wrong: missing text, wrong colors, black areas, garbled rendering
   - If the user specified what to look for (the argument), address that specifically

5. **Report your findings** clearly, including the image path for reference.
