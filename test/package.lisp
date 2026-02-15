(defpackage :mcclim-render-stack-tests
  (:use :cl :parachute :mcclim-render-stack)
  (:import-from :clim
                #:graft-width
                #:graft-height))

(in-package :mcclim-render-stack-tests)

(define-test mcclim-render-stack-suite
  "Top-level test suite for mcclim-render-stack backend.")
