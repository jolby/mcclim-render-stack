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
               :log4cl)
  :components ((:module "src"
                 :components ((:file "package")
                              (:file "pointer" :depends-on ("package"))
                              (:file "port" :depends-on ("package" "pointer"))
                              (:file "medium" :depends-on ("port"))
                              (:file "graft" :depends-on ("port"))
                              (:file "frame-manager" :depends-on ("graft"))
                              (:file "render-delegate" :depends-on ("port")))))
  :in-order-to ((test-op (test-op "mcclim-render-stack/tests"))))

(asdf:defsystem :mcclim-render-stack/tests
  :depends-on (:mcclim-render-stack
               :parachute
               :uiop)
  :components ((:module "test"
                   :components ((:file "package")
                                (:file "test-utils" :depends-on ("package"))
                                (:file "backend-tests" :depends-on ("package" "test-utils"))
                                (:file "ink-conversion-tests" :depends-on ("package" "test-utils" "backend-tests")))))
  :perform (test-op (op c)
                    (uiop:symbol-call :parachute :test :mcclim-render-stack-tests)))

(asdf:defsystem :mcclim-render-stack-examples
  :depends-on (:mcclim-render-stack)
  :components ((:module "examples"
                  :components ((:file "hello-world")))))
