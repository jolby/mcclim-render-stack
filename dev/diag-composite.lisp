;; dev/diag-composite.lisp -- Diagnostic scratchpad for the composite render path
;;
;; Instruments medium-finish-output and composite-pane-dls-for-mirror
;; to print concrete values to *error-output* so we can see exactly
;; what's happening when the black-window bug occurs.
;;
;; Usage:
;;   (load "dev/diag-composite.lisp")
;;   (rs-hello-world:run)   ; or whichever demo
;;
;; Then look at stderr for [DIAG] lines.

(ql:quickload :mcclim-render-stack :silent t :force t)

(cffi:define-foreign-library
    (sdl3-clawed
     :search-path (asdf:system-relative-pathname :render-stack-sdl3-ffi
                                                 "src/lib/build/desktop/"))
  (:unix "libsdl3.clawed.so"))
(cffi:load-foreign-library 'sdl3-clawed)

(in-package :mcclim-render-stack)

;;; ---- Instrument medium-finish-output ----
(defmethod medium-finish-output :before ((medium render-stack-medium))
  (let* ((sheet (medium-sheet medium))
         (builder (medium-display-list-builder medium)))
    (format *error-output*
            "~&[DIAG] medium-finish-output: sheet=~A builder=~A~%"
            (type-of sheet) (if builder "NON-NIL" "NIL"))
    (force-output *error-output*)))

(defmethod medium-finish-output :after ((medium render-stack-medium))
  (let* ((sheet (medium-sheet medium))
         (mirror (when sheet (climi::sheet-mirror sheet))))
    (when (typep mirror 'render-stack-mirror)
      (bt2:with-lock-held ((mirror-dl-lock mirror))
        (let ((map-count (hash-table-count (mirror-pane-dl-map mirror)))
              (dirty-p (mirror-frame-dirty-p mirror)))
          (format *error-output*
                  "~&[DIAG] medium-finish-output AFTER: pane-dl-map entries=~A frame-dirty-p=~A~%"
                  map-count dirty-p)
          (force-output *error-output*))))))

;;; ---- Instrument composite-pane-dls-for-mirror ----
(defun composite-pane-dls-for-mirror (mirror snapshot)
  "INSTRUMENTED version for diagnostics -- replaces the original."
  (format *error-output*
          "~&[DIAG] composite-pane-dls-for-mirror: snapshot-count=~A~%"
          (length snapshot))
  (force-output *error-output*)
  (when (null snapshot)
    (format *error-output* "~&[DIAG] composite: snapshot empty - returning nil~%")
    (force-output *error-output*)
    (return-from composite-pane-dls-for-mirror nil))
  (let* ((phys-w  (mirror-width  mirror))
         (phys-h  (mirror-height mirror))
         (log-w   (mirror-logical-width  mirror))
         (log-h   (mirror-logical-height mirror))
         (scale-x (if (plusp log-w) (float (/ phys-w log-w) 1.0f0) 1.0f0))
         (scale-y (if (plusp log-h) (float (/ phys-h log-h) 1.0f0) 1.0f0)))
    (format *error-output*
            "~&[DIAG] composite: mirror phys=~Ax~A log=~Ax~A scale=~Ax~A~%"
            phys-w phys-h log-w log-h scale-x scale-y)
    (force-output *error-output*)
    (handler-case
        (let ((builder (frs:make-display-list-builder)))
          (unwind-protect
              (progn
                (dolist (entry snapshot)
                  (let ((sheet (car entry))
                        (dl    (cdr entry)))
                    (format *error-output*
                            "~&[DIAG] pane DL ptr null=~A~%"
                            (cffi:null-pointer-p dl))
                    (force-output *error-output*)
                    (handler-case
                        (multiple-value-bind (lx ly)
                            (clim:transform-position
                             (climi::sheet-native-transformation sheet) 0 0)
                          (multiple-value-bind (rx1 ry1 rx2 ry2)
                              (clim:bounding-rectangle* (clim:sheet-region sheet))
                            (let ((pw (float (* (- rx2 rx1) scale-x) 1.0f0))
                                  (ph (float (* (- ry2 ry1) scale-y) 1.0f0))
                                  (px (float (* lx scale-x) 1.0f0))
                                  (py (float (* ly scale-y) 1.0f0)))
                              (format *error-output*
                                      "~&[DIAG] pane ~A: lx=~A ly=~A rx1=~A ry1=~A rx2=~A ry2=~A px=~A py=~A pw=~A ph=~A~%"
                                      (type-of sheet) lx ly rx1 ry1 rx2 ry2 px py pw ph)
                              (force-output *error-output*)
                              (if (and (plusp pw) (plusp ph))
                                  (progn
                                    (format *error-output* "~&[DIAG] pane ~A: DRAWING~%" (type-of sheet))
                                    (force-output *error-output*)
                                    (frs:display-list-builder-save builder)
                                    (frs:display-list-builder-translate builder px py)
                                    (frs:display-list-builder-clip-rect builder 0.0 0.0 pw ph)
                                    (frs:display-list-builder-draw-display-list builder dl 1.0)
                                    (frs:display-list-builder-restore builder))
                                  (progn
                                    (format *error-output* "~&[DIAG] pane ~A: SKIPPED (zero size)~%" (type-of sheet))
                                    (force-output *error-output*))))))
                      (error (e)
                        (format *error-output*
                                "~&[DIAG] pane ~A positioning ERROR: ~A~%"
                                (type-of sheet) e)
                        (force-output *error-output*)))))
                ;; SANITY CHECK A: draw a bright red rect directly (no draw-display-list).
                ;; If this shows, the composite builder renders directly.
                (let ((test-paint (frs:make-paint)))
                  (frs:paint-set-color test-paint 1.0 0.0 0.0 1.0)
                  (frs:draw-rect builder 5.0 5.0 80.0 80.0 test-paint)
                  (frs:release-paint test-paint))
                (format *error-output* "~&[DIAG] composite: direct red rect added~%")
                (force-output *error-output*)
                ;; SANITY CHECK B: draw a FRESH SIMPLE DL via draw-display-list (no set-transform).
                ;; A green rect at (200,200). If this shows but pane DLs don't,
                ;; the issue is set-transform inside the pane DLs.
                (let* ((simple-builder (frs:make-display-list-builder))
                       (simple-paint   (frs:make-paint)))
                  (frs:paint-set-color simple-paint 0.0 1.0 0.0 1.0)
                  (frs:draw-rect simple-builder 200.0 200.0 80.0 80.0 simple-paint)
                  (frs:release-paint simple-paint)
                  (let ((simple-dl (frs:create-display-list simple-builder)))
                    (frs:release-display-list-builder simple-builder)
                    (frs:display-list-builder-draw-display-list builder simple-dl 1.0)
                    (frs:release-display-list simple-dl)))
                (format *error-output* "~&[DIAG] composite: green rect via draw-display-list added~%")
                (force-output *error-output*)
                (frs:create-display-list builder))
            (frs:release-display-list-builder builder)))
      (error (e)
        (format *error-output* "~&[DIAG] composite build ERROR: ~A~%" e)
        (force-output *error-output*)
        nil))))

;;; ---- Instrument render-delegate-draw at the dirty check ----
(defmethod render-stack:render-delegate-draw :before
    ((runtime render-stack-runtime) pipeline-item)
  (declare (ignore pipeline-item))
  (let ((port (runtime-port runtime)))
    (when port
      (let ((mirrors (bt2:with-lock-held ((port-registry-lock port))
                       (let (ms)
                         (maphash (lambda (id m) (declare (ignore id)) (push m ms))
                                  (port-window-registry port))
                         ms))))
        (dolist (mirror mirrors)
          (let ((dirty (bt2:with-lock-held ((mirror-dl-lock mirror))
                         (mirror-frame-dirty-p mirror)))
                (first-drawn (mirror-first-frame-drawn-p mirror))
                (map-count (bt2:with-lock-held ((mirror-dl-lock mirror))
                             (hash-table-count (mirror-pane-dl-map mirror)))))
            (format *error-output*
                    "~&[DIAG] render-delegate-draw: first-drawn=~A dirty=~A pane-dl-map-count=~A~%"
                    first-drawn dirty map-count)
            (force-output *error-output*)))))))

(format *error-output* "~&[DIAG] Instrumentation loaded. Run your demo now.~%")
(force-output *error-output*)

;;; ---- Quick demo runner ----
(defpackage :rs-diag
  (:use :clim :clim-lisp)
  (:export #:run))

(in-package :rs-diag)

(define-application-frame diag-app ()
  ()
  (:panes
   (canvas :application
           :display-function (lambda (frame pane)
                               (declare (ignore frame))
                               (let ((w (bounding-rectangle-width pane))
                                     (h (bounding-rectangle-height pane)))
                                 (draw-rectangle* pane 0 0 w h :filled t :ink clim:+grey90+)
                                 (draw-rectangle* pane 50 50 200 150 :filled t :ink clim:+blue+)
                                 (draw-text* pane "Diag Test" (/ w 2) (/ h 2)
                                             :align-x :center :align-y :center)
                                 (finish-output pane)))
           :scroll-bars nil
           :min-width 400
           :min-height 300))
  (:layouts (default canvas))
  (:menu-bar nil)
  (:command-table (diag-app :inherit-from nil)))

(defun run ()
  (setf clim:*default-server-path* '(:render-stack))
  (clim:run-frame-top-level
   (clim:make-application-frame 'diag-app :width 500 :height 400)))
