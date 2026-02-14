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
               :fiveam)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "backend-tests" :depends-on ("package")))))
  :perform (test-op (op c) (symbol-call :fiveam :run! :mcclim-render-stack)))
