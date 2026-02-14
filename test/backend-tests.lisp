;;;; backend-tests.lisp — Tests for the mcclim-render-stack backend

(in-package :mcclim-render-stack-tests)

;;; Basic tests

(define-test server-path-registered
  :parent mcclim-render-stack-suite
  "Test that the :render-stack server path is registered."
  (true (not (null (get :render-stack :port-type)))))

(define-test port-creation
  :parent mcclim-render-stack-suite
  "Test basic port creation."
  (let ((port (make-instance 'render-stack-port)))
    (true (typep port 'render-stack-port))
    (true (null (render-stack-port-pipeline port)))))

(define-test medium-creation
  :parent mcclim-render-stack-suite
  "Test basic medium creation."
  (let ((medium (make-instance 'render-stack-medium)))
    (true (typep medium 'render-stack-medium))))

(define-test graft-dimensions
  :parent mcclim-render-stack-suite
  "Test graft dimension queries."
  (let ((graft (make-instance 'render-stack-graft)))
    (true (numberp (graft-width graft)))
    (true (numberp (graft-height graft)))
    (true (plusp (graft-width graft)))
    (true (plusp (graft-height graft)))))

;;; Phase 1 Integration Test

(defun run-phase-1-visual-test ()
  "Run a visual integration test for Phase 1 drawing operations.

   This function creates a render-stack-port and draws a test pattern
   to verify all Phase 1 operations work correctly:
   - Rectangle drawing (filled and stroked)
   - Line drawing
   - Polygon drawing (triangle)
   - Clear area
   - Line styles (thickness)

   The test pattern includes:
   - Gray background (via clear-area)
   - Filled red rectangle
   - Stroked blue rectangle with thick border
   - Green lines forming a cross
   - Yellow triangle (filled polygon)

   Usage:
     (mcclim-render-stack-tests:run-phase-1-visual-test)

   Note: This is a manual visual test. The window will stay open until
   you close it or call (clim:destroy-port port)."
  (format t "~%Starting Phase 1 visual integration test...~%")
  (format t "Creating render-stack-port...~%")

  (let ((port (make-instance 'render-stack-port)))
    (format t "Port created successfully.~%")
    (format t "Test pattern should appear in the window.~%")
    (format t "Close the window to end the test.~%")

    ;; Get the delegate's window and draw test pattern
    (let* ((delegate (port-delegate port))
           (window (delegate-window delegate)))

      ;; Override the draw method temporarily to draw our test pattern
      (defmethod render-delegate-draw :before ((del clim-render-delegate) pipeline-item)
        (declare (ignore pipeline-item))
        (let ((builder (delegate-current-builder del)))
          (when builder
            ;; Draw test pattern
            (draw-phase-1-test-pattern builder))))

      ;; Wait a bit for the window to appear and render
      (sleep 0.5)

      port)))

(defun draw-phase-1-test-pattern (builder)
  "Draw the Phase 1 test pattern to the given display list builder.

   This function demonstrates all Phase 1 drawing operations:
   - Clear area (gray background)
   - Filled rectangles
   - Stroked rectangles with line styles
   - Lines
   - Filled polygons"
  (let ((paint (frs:make-paint)))
    (unwind-protect
         (progn
           ;; 1. Clear to gray background
           (frs:paint-set-color paint 0.9 0.9 0.9 1.0)  ; Light gray
           (frs:paint-set-draw-style paint :fill)
           (frs:draw-rect builder 0 0 800 600 paint)

           ;; 2. Filled red rectangle
           (frs:paint-set-color paint 0.9 0.2 0.2 1.0)  ; Red
           (frs:draw-rect builder 50 50 200 150 paint)

           ;; 3. Stroked blue rectangle with thick border
           (frs:paint-set-color paint 0.2 0.4 0.9 1.0)  ; Blue
           (frs:paint-set-draw-style paint :stroke)
           (frs:paint-set-stroke-width paint 5.0)
           (frs:draw-rect builder 300 50 200 150 paint)

           ;; 4. Green lines forming a cross
           (frs:paint-set-color paint 0.2 0.8 0.2 1.0)  ; Green
           (frs:paint-set-stroke-width paint 3.0)
           (frs:with-path-builder (pb)
             ;; Horizontal line
             (frs:path-move-to pb 50 300)
             (frs:path-line-to pb 250 300)
             ;; Vertical line
             (frs:path-move-to pb 150 250)
             (frs:path-line-to pb 150 350)
             (let ((path (frs:build-path pb)))
               (unwind-protect
                    (frs:draw-path builder path paint)
                 (frs:release-path path))))

           ;; 5. Yellow triangle (filled polygon)
           (frs:paint-set-color paint 0.9 0.9 0.2 1.0)  ; Yellow
           (frs:paint-set-draw-style paint :fill)
           (frs:with-path-builder (pb)
             (frs:path-move-to pb 400 300)   ; Top
             (frs:path-line-to pb 350 400)   ; Bottom left
             (frs:path-line-to pb 450 400)   ; Bottom right
             (frs:path-close pb)             ; Close the triangle
             (let ((path (frs:build-path pb)))
               (unwind-protect
                    (frs:draw-path builder path paint)
                 (frs:release-path path))))

           ;; 6. Label text (using simple rectangles as placeholder for now)
           ;; Phase 2 will add real text support
           (frs:paint-set-color paint 0.0 0.0 0.0 1.0)  ; Black
           (frs:paint-set-draw-style paint :fill)
           ;; Just draw small rectangles to indicate text positions
           (frs:draw-rect builder 60 210 5 5 paint)   ; Red rect label
           (frs:draw-rect builder 310 210 5 5 paint)  ; Blue rect label
           (frs:draw-rect builder 60 360 5 5 paint)   ; Green cross label
           (frs:draw-rect builder 390 410 5 5 paint)) ; Yellow triangle label

      (frs:release-paint paint))))

(define-test phase-1-visual-test-exists
  :parent mcclim-render-stack-suite
  "Test that the Phase 1 visual test function exists."
  (true (fboundp 'run-phase-1-visual-test))
  (true (fboundp 'draw-phase-1-test-pattern)))
