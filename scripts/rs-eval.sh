#!/usr/bin/env bash
# scripts/rs-eval.sh -- Send a Lisp expression to the rs-repl-server and print the result.
#
# Usage:
#   scripts/rs-eval.sh 'LISP-EXPR'          # expression as argument
#   echo 'LISP-EXPR' | scripts/rs-eval.sh   # expression from stdin
#
# Environment:
#   RS_SERVER_DIR   -- server directory (default /tmp/rs-server)
#   RS_EVAL_TIMEOUT -- max seconds to wait for result (default 60)
#
# Exit codes:
#   0 -- success (result printed to stdout)
#   1 -- timeout or server not running
#   2 -- usage error

set -euo pipefail

DIR="${RS_SERVER_DIR:-/tmp/rs-server}"
TIMEOUT="${RS_EVAL_TIMEOUT:-60}"

if [ $# -gt 1 ]; then
  echo "Usage: rs-eval.sh 'EXPR'  or  echo EXPR | rs-eval.sh" >&2
  exit 2
fi

# Accept expression from argument or stdin
if [ $# -eq 1 ]; then
  EXPR="$1"
else
  EXPR="$(cat)"
fi

# Check server directory exists
if [ ! -d "$DIR" ]; then
  echo "ERROR: Server directory $DIR not found. Start the server:" >&2
  echo "  (rs-test-rig:start-repl-server) in your REPL" >&2
  exit 1
fi

# Clean up any previous transaction state
rm -f "$DIR/cmd.ready" "$DIR/result.done" "$DIR/result.sexp"

# Write command and signal ready
printf '%s\n' "$EXPR" > "$DIR/cmd.sexp"
touch "$DIR/cmd.ready"

# Poll for result
END=$((SECONDS + TIMEOUT))
while [ "$SECONDS" -lt "$END" ]; do
  if [ -f "$DIR/result.done" ]; then
    cat "$DIR/result.sexp"
    exit 0
  fi
  sleep 0.1
done

echo "ERROR: Timeout after ${TIMEOUT}s. Is the demo still running?" >&2
exit 1
