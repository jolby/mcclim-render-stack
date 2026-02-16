SHELL := /bin/sh

CWD := $(shell pwd)
SBCL ?= sbcl
QL ?= $(HOME)/quicklisp/setup.lisp

SBCL_FLAGS := --dynamic-space-size 8096 --noinform --disable-debugger --non-interactive
ASDF_BOOT := --eval '(require :asdf)' --eval "(pushnew \#p\"$(CWD)/\" asdf:*central-registry*)"
QL_BOOT := --eval '(load "$(QL)")'

.PHONY: all load test test-unit test-integration test-ink test-line-style test-event clean check-quicklisp demo

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
	  --eval '(ql:quickload :mcclim-render-stack/examples :force t :silent t)' \
	  --eval '(load "$(CWD)/examples/hello-world.lisp")' \
	  --eval '(clim-user::hello-world-run)'
