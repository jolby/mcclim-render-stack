(defpackage :mcclim-render-stack-tests
  (:use :cl :parachute :mcclim-render-stack)
  (:import-from :clim
                #:graft-width
                #:graft-height
                #:make-line-style
                #:line-style-p
                #:line-style-thickness
                #:line-style-cap-shape
                #:line-style-joint-shape
                #:make-text-style
                #:text-style-p
                #:text-style-family
                #:text-style-size
                #:text-style-face
                #:text-style-components
                #:medium-text-style))

