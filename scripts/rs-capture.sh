#!/usr/bin/env bash
# scripts/rs-capture.sh -- Capture a rendered frame and output a PNG path.
#
# Usage:
#   scripts/rs-capture.sh [FRAMES] [PPM-OUTPUT]
#
# Arguments:
#   FRAMES      -- number of frames to advance before capture (default 15)
#   PPM-OUTPUT  -- path for the PPM file (default /tmp/rs-snapshot.ppm)
#
# The PPM is converted to PNG (same path with .png extension) using ImageMagick.
# The PNG path is printed to stdout for use by Claude Code skills.
#
# The rs-repl-server must be running in the demo:
#   (rs-test-rig:start-repl-server) in your REPL

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRAMES="${1:-15}"
PPM_OUTPUT="${2:-/tmp/rs-snapshot.ppm}"
PNG_OUTPUT="${PPM_OUTPUT%.ppm}.png"

# Run capture via eval server
"$SCRIPT_DIR/rs-eval.sh" \
  "(rs-test-rig:capture-frame :frames $FRAMES :output \"$PPM_OUTPUT\")" \
  >/dev/null

# Verify PPM was created
if [ ! -f "$PPM_OUTPUT" ]; then
  echo "ERROR: PPM not created at $PPM_OUTPUT" >&2
  exit 1
fi

# Convert PPM -> PNG for Claude Code image reading
if command -v magick >/dev/null 2>&1; then
  magick "$PPM_OUTPUT" "$PNG_OUTPUT"
elif command -v convert >/dev/null 2>&1; then
  convert "$PPM_OUTPUT" "$PNG_OUTPUT"
else
  echo "ERROR: ImageMagick not found. Install it: sudo zypper install ImageMagick" >&2
  echo "  (PPM saved at $PPM_OUTPUT -- convert manually)" >&2
  exit 1
fi

echo "$PNG_OUTPUT"
