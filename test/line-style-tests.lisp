;;;; line-style-tests.lisp — Unit tests for line style configuration

(in-package :mcclim-render-stack-tests)

(define-test configure-paint-for-stroke-exists
  :parent mcclim-render-stack-suite
  "Test that configure-paint-for-stroke function exists and is callable."
  (true (fboundp 'mcclim-render-stack::configure-paint-for-stroke)))

(define-test make-line-style-functionality
  :parent mcclim-render-stack-suite
  "Test that line styles can be created with make-line-style."
  (let ((line-style (make-line-style :thickness 2
                                     :cap-shape :round
                                     :joint-shape :miter)))
    (true (line-style-p line-style))
    (is = 2 (line-style-thickness line-style))
    (is eq :round (line-style-cap-shape line-style))
    (is eq :miter (line-style-joint-shape line-style))))

(define-test line-style-cap-shape-mapping
  :parent mcclim-render-stack-suite
  "Test cap shape values: :butt, :round, :square, :no-end-point."
  (let ((butt-style (make-line-style :cap-shape :butt))
        (round-style (make-line-style :cap-shape :round))
        (square-style (make-line-style :cap-shape :square))
        (no-end-point-style (make-line-style :cap-shape :no-end-point)))
    (is eq :butt (line-style-cap-shape butt-style))
    (is eq :round (line-style-cap-shape round-style))
    (is eq :square (line-style-cap-shape square-style))
    (is eq :no-end-point (line-style-cap-shape no-end-point-style))))

(define-test line-style-joint-shape-mapping
  :parent mcclim-render-stack-suite
  "Test joint shape values: :miter, :round, :bevel, :none."
  (let ((miter-style (make-line-style :joint-shape :miter))
        (round-style (make-line-style :joint-shape :round))
        (bevel-style (make-line-style :joint-shape :bevel))
        (none-style (make-line-style :joint-shape :none)))
    (is eq :miter (line-style-joint-shape miter-style))
    (is eq :round (line-style-joint-shape round-style))
    (is eq :bevel (line-style-joint-shape bevel-style))
    (is eq :none (line-style-joint-shape none-style))))

(define-test line-style-thickness
  :parent mcclim-render-stack-suite
  "Test line style thickness values."
  (let ((thin-style (make-line-style :thickness 1))
        (medium-style (make-line-style :thickness 3))
        (thick-style (make-line-style :thickness 10)))
    (is = 1 (line-style-thickness thin-style))
    (is = 3 (line-style-thickness medium-style))
    (is = 10 (line-style-thickness thick-style))))

(define-test configure-paint-with-nil-line-style
  :parent mcclim-render-stack-suite
  "Test configure-paint-for-stroke handles nil line-style (uses defaults)."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port))
           (paint (frs:make-paint)))
      (unwind-protect
           (progn
             (mcclim-render-stack::configure-paint-for-stroke paint nil medium)
             (true t))
        (frs:release-paint paint)
        (clim:destroy-port port)))))

(define-test configure-paint-with-default-line-style
  :parent mcclim-render-stack-suite
  "Test configure-paint-for-stroke with a default line style."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port))
           (paint (frs:make-paint))
           (line-style (make-line-style :thickness 2
                                      :cap-shape :round
                                      :joint-shape :round)))
      (unwind-protect
           (progn
             (mcclim-render-stack::configure-paint-for-stroke paint line-style medium)
             (true t))
        (frs:release-paint paint)
        (clim:destroy-port port)))))

(define-test configure-paint-cap-shape-mappings
  :parent mcclim-render-stack-suite
  "Test configure-paint-for-stroke works with different cap shapes."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port)))
      (dolist (cap-shape '(:butt :round :square :no-end-point))
        (let* ((paint (frs:make-paint))
               (line-style (make-line-style :cap-shape cap-shape)))
          (unwind-protect
               (progn
                 (mcclim-render-stack::configure-paint-for-stroke paint line-style medium)
                 (true t))
            (frs:release-paint paint))))
      (clim:destroy-port port))))

(define-test configure-paint-joint-shape-mappings
  :parent mcclim-render-stack-suite
  "Test configure-paint-for-stroke works with different joint shapes."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port)))
      (dolist (joint-shape '(:miter :round :bevel :none))
        (let* ((paint (frs:make-paint))
               (line-style (make-line-style :joint-shape joint-shape)))
          (unwind-protect
               (progn
                 (mcclim-render-stack::configure-paint-for-stroke paint line-style medium)
                 (true t))
            (frs:release-paint paint))))
      (clim:destroy-port port))))

(define-test configure-paint-thickness-values
  :parent mcclim-render-stack-suite
  "Test configure-paint-for-stroke works with different thickness values."
  (skip-unless-sdl3
    (let* ((port (make-instance 'render-stack-port))
           (medium (make-instance 'render-stack-medium :port port)))
      (dolist (thickness '(1 2 5 10))
        (let* ((paint (frs:make-paint))
               (line-style (make-line-style :thickness thickness)))
          (unwind-protect
               (progn
                 (mcclim-render-stack::configure-paint-for-stroke paint line-style medium)
                 (true t))
            (frs:release-paint paint))))
      (clim:destroy-port port))))
