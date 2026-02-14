(in-package :mcclim-render-stack)

;;; Medium - the drawing surface abstraction

(defclass render-stack-medium (basic-medium)
  ((paint :accessor medium-paint :initform nil
          :documentation "Persistent Impeller paint object for this medium.")
   (surface :accessor medium-surface :initform nil
            :documentation "The Impeller rendering surface."))
  (:documentation "McCLIM medium using Impeller for drawing."))

;;; Ink Conversion Utilities
;;; Converts CLIM inks (colors, +foreground-ink+, +background-ink+, etc.)
;;; to Impeller RGBA color values (floats 0.0-1.0).

(defun clim-ink-to-impeller-color (ink medium)
  "Convert a CLIM ink to Impeller RGBA color values.

   Arguments:
     ink    - A CLIM ink (color, +foreground-ink+, +background-ink+, opacity)
     medium - The medium providing foreground/background context

   Returns four float values: red, green, blue, alpha (each 0.0-1.0).

   Handles:
     - Standard colors (make-rgb-color results)
     - Named colors (+red+, +blue+, +black+, +white+, etc.)
     - +foreground-ink+ -> medium's foreground color
     - +background-ink+ -> medium's background color
     - Opacity objects -> foreground with specified alpha
     - Composited inks -> extracts RGBA via color-rgba"
  (cond
    ((eq ink clim:+foreground-ink+)
     (clim-ink-to-impeller-color
      (medium-foreground medium) medium))
    ((eq ink clim:+background-ink+)
     (clim-ink-to-impeller-color
      (medium-background medium) medium))
    ((null ink)
     (values 0.0f0 0.0f0 0.0f0 1.0f0))
    ((typep ink 'clim:color)
     (multiple-value-bind (r g b a)
         (clime:color-rgba ink)
       (values (float r 1.0f0) (float g 1.0f0) (float b 1.0f0) (float a 1.0f0))))
    (t
     ;; Fallback for flipping-ink and other designs (like patterns)
     ;; For now, we just use the medium's foreground color.
     ;; This avoids crashing when drawing things like the interactor cursor.
     (clim-ink-to-impeller-color (medium-foreground medium) medium))))

(defun set-paint-from-ink (paint ink medium)
  "Configure an Impeller paint object from a CLIM ink.

   Arguments:
     paint  - Impeller paint object to configure
     ink    - CLIM ink to use for color
     medium - Medium providing foreground/background context

   Returns a color source if a gradient was used (caller must release it),
   or NIL for solid colors. Modifies paint in place.

   Note: Does NOT clear previous color sources - the with-ink-on-paint macro
   handles cleanup only when a color source was actually used."
  ;; For Phase 1: Only solid colors are supported
  ;; Gradients will be added in Phase 5
  (multiple-value-bind (r g b a)
      (clim-ink-to-impeller-color ink medium)
    (frs:paint-set-color paint r g b a))
  nil)

(defmacro with-ink-on-paint ((paint ink medium) &body body)
  "Configure paint from ink, execute body, cleanup any gradient color sources.

   This macro handles the lifecycle of Impeller color sources automatically:
   - For solid colors: just sets the color
   - For gradients: creates color source, draws, then releases it

   Phase 1: Solid colors only. Gradient support will be added in Phase 5."
  (let ((color-source (gensym "COLOR-SOURCE")))
    `(let ((,color-source (set-paint-from-ink ,paint ,ink ,medium)))
       (unwind-protect
            (progn ,@body)
         (when ,color-source
           (frs:release-color-source ,color-source))))))

;;; Line Style Support

(defun configure-paint-for-stroke (paint line-style medium)
  "Configure paint for stroke drawing based on CLIM line style.

   Arguments:
     paint      - Impeller paint object
     line-style - CLIM line style (or nil for default)
     medium     - The medium (for computing effective thickness)

   Maps CLIM line properties to Impeller paint settings:
   - Thickness -> stroke-width
   - Cap shape -> stroke-cap (:butt, :round, :square)
   - Join shape -> stroke-join (:miter, :round, :bevel)"
  (let ((thickness (clime:line-style-effective-thickness
                    (or line-style (medium-line-style medium))
                    medium)))
    (frs:paint-set-draw-style paint :stroke)
    (frs:paint-set-stroke-width paint (float thickness 1.0f0))
    ;; Map CLIM line cap to Impeller
    ;; CLIM: :butt, :round, :square, :no-end-point
    ;; Impeller: :butt, :round, :square
    (frs:paint-set-stroke-cap
     paint
     (if line-style
         (case (line-style-cap-shape line-style)
           (:butt :butt)
           (:round :round)
           (:square :square)
           (:no-end-point :butt)  ; closest match
           (t :butt))
         :butt))
    ;; Map CLIM line join to Impeller
    ;; CLIM: :miter, :round, :bevel, :none
    ;; Impeller: :miter, :round, :bevel
    (let ((join (if line-style
                    (line-style-joint-shape line-style)
                    :miter)))
      (frs:paint-set-stroke-join
       paint
       (case join
         (:miter :miter)
         (:round :round)
         (:bevel :bevel)
         (:none :bevel)  ; closest match
         (t :miter))))))

;;; Medium drawing operations
;;; These will be implemented using flutter-render-stack (Impeller)

(defmethod medium-draw-point* ((medium render-stack-medium) x y)
  ;; Draw a point at (x, y)
  (declare (ignore x y)))

(defmethod medium-draw-points* ((medium render-stack-medium) coord-seq)
  ;; Draw multiple points
  (declare (ignore coord-seq)))

(defmethod medium-draw-line* ((medium render-stack-medium) x1 y1 x2 y2)
  ;; Draw a line from (x1, y1) to (x2, y2)
  (declare (ignore x1 y1 x2 y2)))

(defmethod medium-draw-lines* ((medium render-stack-medium) coord-seq)
  ;; Draw multiple lines
  (declare (ignore coord-seq)))

(defmethod medium-draw-polygon* ((medium render-stack-medium) coord-seq closed filled)
  ;; Draw a polygon
  (declare (ignore coord-seq closed filled)))

(defmethod medium-draw-rectangle* ((medium render-stack-medium) x1 y1 x2 y2 filled)
  ;; Draw a rectangle
  (declare (ignore x1 y1 x2 y2 filled)))

(defmethod medium-draw-ellipse* ((medium render-stack-medium)
                                  center-x center-y
                                  radius-1-dx radius-1-dy
                                  radius-2-dx radius-2-dy
                                  start-angle end-angle filled)
  ;; Draw an ellipse or arc
  (declare (ignore center-x center-y
                   radius-1-dx radius-1-dy
                   radius-2-dx radius-2-dy
                   start-angle end-angle filled)))

(defmethod medium-draw-text* ((medium render-stack-medium)
                               string x y
                               start end
                               align-x align-y
                               toward-x toward-y transform-glyphs)
  ;; Draw text
  (declare (ignore string x y start end
                   align-x align-y toward-x toward-y transform-glyphs)))

;;; Medium state

(defmethod medium-finish-output ((medium render-stack-medium))
  ;; Ensure all drawing is complete
  )

(defmethod medium-force-output ((medium render-stack-medium))
  ;; Flush pending drawing operations
  )

(defmethod medium-clear-area ((medium render-stack-medium) left top right bottom)
  ;; Clear a rectangular area
  (declare (ignore left top right bottom)))

(defmethod medium-beep ((medium render-stack-medium))
  ;; Produce an audible beep
  )
