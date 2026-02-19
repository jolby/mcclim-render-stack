(asdf:defsystem :mcclim-render-stack
  :description "McCLIM backend using render-stack (Flutter Impeller + SDL3)"
  :version "0.1.0"
  :author "Joel Boehland"
  :license "MIT"
  :depends-on (:mcclim
               :render-stack
               :render-stack-sdl3
               :flutter-render-stack
               :alexandria
               :verbose
               :bordeaux-threads)
  :components ((:module "src"
                  :components ((:file "package")
                               (:file "runtime" :depends-on ("package"))
                               (:file "multi-window-delegate" :depends-on ("package"))
                               (:file "globals" :depends-on ("package" "multi-window-delegate"))
                               (:file "runner-phases" :depends-on ("package" "multi-window-delegate" "globals" "runtime"))
                               (:file "pointer" :depends-on ("package"))
                               (:file "port" :depends-on ("package" "pointer" "multi-window-delegate" "globals" "runner-phases" "runtime"))
                               (:file "medium" :depends-on ("port"))
                               (:file "graft" :depends-on ("port"))
                               (:file "frame-manager" :depends-on ("graft"))
                               )))
  :in-order-to ((asdf:test-op (asdf:test-op "mcclim-render-stack/tests"))))

(asdf:defsystem :mcclim-render-stack/tests
  :depends-on (:mcclim-render-stack
               :parachute
               :uiop
               :lparallel)
  :components ((:module "test"
                    :components ((:file "package")
                                 (:file "suites" :depends-on ("package"))
                                 (:file "test-utils" :depends-on ("package" "suites"))
                                 (:file "backend-tests" :depends-on ("package" "suites" "test-utils"))
                                 (:file "ink-conversion-tests" :depends-on ("package" "suites" "test-utils" "backend-tests"))
                                  (:file "test-multi-window-delegate" :depends-on ("package" "suites" "test-utils"))
                                  ;; bd-3hi.9: Tests for Task 1.5
                                  (:file "test-global-engine" :depends-on ("package" "suites" "test-utils"))
                                  ;; bd-3hi.9: Tests for Task 1.6
                                  (:file "test-port-refactor" :depends-on ("package" "suites" "test-utils"))
                                  ;; bd-3hi.10: Integration Tests
                                  (:file "test-integration" :depends-on ("package" "suites" "test-utils" "test-global-engine" "test-port-refactor"))
                                  )))
  :perform (asdf:test-op (op c)
                     (uiop:symbol-call :parachute :test :mcclim-render-stack-tests)))

(asdf:defsystem :mcclim-render-stack/examples
  :depends-on (:mcclim-render-stack)
  :components ((:module "examples"
                  :components ((:file "hello-world")))))
