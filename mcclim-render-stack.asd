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
                               (:file "events"        :depends-on ("package" "pointer"))
                               (:file "mirror"        :depends-on ("package" "events"))
                               (:file "runtime"       :depends-on ("package" "mirror"))
                               (:file "runner-phases" :depends-on ("package" "runtime" "events"))
                               (:file "port"          :depends-on ("package" "pointer" "mirror" "runtime" "runner-phases"))
                               (:file "pointer"       :depends-on ("package"))
                               (:file "medium"        :depends-on ("port"))
                               (:file "graft"         :depends-on ("port"))
                               (:file "frame-manager" :depends-on ("graft"))
                               )))
  :in-order-to ((asdf:test-op (asdf:test-op "mcclim-render-stack/tests"))))

(asdf:defsystem :mcclim-render-stack/tests
  :depends-on (:mcclim-render-stack
               :parachute
               :lparallel)
  :components ((:module "test"
                    :components ((:file "package")
                                 (:file "suites"     :depends-on ("package"))
                                 (:file "test-utils" :depends-on ("package" "suites"))
                                 (:file "port-tests" :depends-on ("package" "suites"))
                                 (:file "medium-tests" :depends-on ("package" "suites"))
                                 (:file "pointer-tests" :depends-on ("package" "suites"))
                                 (:file "keyboard-event-tests" :depends-on ("package" "suites"))
                                 (:file "ink-conversion-tests" :depends-on ("package" "suites"))
                                 (:file "line-style-tests" :depends-on ("package" "suites"))
                                 (:file "text-style-tests" :depends-on ("package" "suites"))
                                 (:file "font-metrics-tests" :depends-on ("package" "suites"))
                                 )))
  :perform (asdf:test-op (op c)
                     (uiop:symbol-call :parachute :test :mcclim-render-stack-tests)))

(asdf:defsystem :mcclim-render-stack/examples
  :depends-on (:mcclim-render-stack)
  :components ((:module "examples"
                  :components ((:file "hello-world")))))
