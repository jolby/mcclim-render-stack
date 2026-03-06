SHELL := /bin/sh

CWD := $(shell pwd)
SBCL ?= sbcl
QL ?= $(HOME)/quicklisp/setup.lisp

# Interactive REPL (for debugging etc)
# SBCL_FLAGS := --dynamic-space-size 8096 --noinform
# Normal non-interactive for batch testing etc.
SBCL_FLAGS := --dynamic-space-size 8096 --noinform --disable-debugger --non-interactive
ASDF_BOOT := --eval '(require :asdf)' --eval "(pushnew \#p\"$(CWD)/\" asdf:*central-registry*)"
QL_BOOT := --eval '(load "$(QL)")'

.PHONY: all load test test-unit test-integration test-ink test-line-style test-event clean check-quicklisp demo demo-simple-frame demo-drawing demo-min-button

all: load

check-quicklisp:
	@test -f "$(QL)" || { \
	  echo "Quicklisp not found at $(QL). Override QL=/path/to/quicklisp/setup.lisp"; \
	  exit 1; \
	}

load: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(ql:quickload :mcclim-render-stack :force t :silent t)' \
	  --eval '(format t "~&System loaded successfully.~%")'

test: test-unit

test-unit: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(ql:quickload :mcclim-render-stack :force t :silent t)' \
	  --eval '(ql:quickload :mcclim-render-stack/tests :force t :silent t)' \
	  --eval '(asdf:test-system "mcclim-render-stack")'

clean:
	@find . -type f \( -name "*.fasl" -o -name "*.x86f" -o -name "*.fas" \) -print0 | xargs -0 -r rm -f

demo: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(ql:quickload :mcclim-render-stack :force t :silent t)' \
	  --eval '(load "$(CWD)/examples/hello-world.lisp")' \
	  --eval '(rs-hello-world:run)'

demo-simple-frame: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(ql:quickload :mcclim-render-stack :force t :silent t)' \
	  --eval '(load "$(CWD)/examples/simple-frame.lisp")' \
	  --eval '(rs-simple-frame:run)'

demo-min-button: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(ql:quickload :mcclim-render-stack :force t :silent t)' \
	  --eval '(load "$(CWD)/examples/min-button.lisp")' \
	  --eval '(rs-min-button:run)'

demo-drawing: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(ql:quickload :mcclim-render-stack :force t :silent t)' \
	  --eval '(load "$(CWD)/examples/drawing-demo.lisp")' \
	  --eval '(rs-drawing-demo:run)'

demo-superapp: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(ql:quickload :mcclim-render-stack :force t :silent t)' \
	  --eval '(ql:quickload :mcclim-render-stack/examples :force t :silent t)' \
	  --eval '(load "$(CWD)/examples/superapp.lisp")' \
	  --eval '(clim-demo.app:app-main)'

demo-town: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(ql:quickload :mcclim-render-stack :force t :silent t)' \
	  --eval '(load "$(CWD)/examples/town-example.lisp")' \
	  --eval '(clim-demo.town-example:run)'

demo-updating-output: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(ql:quickload :mcclim-render-stack :force t :silent t)' \
	  --eval '(load "$(CWD)/examples/updating-output-demo.lisp")' \
	  --eval '(updating-app:app-main)'

diag-composite: check-quicklisp
	$(SBCL) $(SBCL_FLAGS) $(QL_BOOT) $(ASDF_BOOT) \
	  --eval '(load "$(CWD)/dev/diag-composite.lisp")' \
	  --eval '(rs-diag:run)'
