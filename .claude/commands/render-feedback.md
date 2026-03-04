Full rendering feedback loop: capture → analyze → diagnose → suggest code changes.

**Usage:** `/render-feedback <expected-behavior-description>`

**Example:** `/render-feedback push-button labeled Hello should show visible text at all times, and darken on hover`

---

## Steps

### 1. Capture baseline
```
scripts/rs-capture.sh 15 /tmp/rs-feedback.ppm
```
If this fails, check: is the demo running? Is the eval server started?
Fallback diagnosis: `scripts/rs-eval.sh '(identity :ping)'`

### 2. Read and describe the image
Read `/tmp/rs-feedback.png`. Describe exactly what you see:
- Background color and pane layout
- Button states (text visible? correct color? hover highlight?)
- Any black areas, missing elements, or garbled content

### 3. Compare against expected behavior
The user's expected behavior is given as the argument to `/render-feedback`.
List each expected property and whether it holds.

### 4. If rendering is INCORRECT, diagnose

Use your knowledge of the rendering stack:

**Key data flow:**
- CLIM repaint → `invoke-with-output-buffered` → `medium-finish-output` on `render-stack-medium`
- Medium builds display list via `medium-display-list-builder` (flutter-render-stack)
- Display list stored in `mirror-pending-dl` via `mirror-store-pending-dl`
- Render thread picks up DL in `render-delegate-draw` (`src/runtime.lisp`)
- Text rendered via Impeller paragraphs; cache in `port-paragraph-cache` (`src/cache.lisp`)
- Hover/press state changes fire CLIM pointer events → pane repaints

**Common failure modes:**
- Missing text → paragraph cache miss / wrong ink color key / Impeller float traps
- Black pane → use-after-free in DL or layer tree (check `mirror-composite-deps`)
- No hover response → pointer events not reaching the sheet / wrong window-id
- Stale render → `invoke-with-output-buffered` not calling `medium-finish-output`

Suggest 1-3 specific hypotheses with file:line pointers.

### 5. Propose next investigative steps
- Log additions to confirm data flow
- Additional `/render-step` captures (e.g., before and after hover)
- Specific variables to inspect in REPL
- Code changes to attempt

### 6. If rendering is CORRECT
Confirm it and summarize what was verified.
