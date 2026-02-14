(in-package :mcclim-render-stack)

;;; Medium - the drawing surface abstraction

(defclass render-stack-medium (basic-medium)
  ((paint :accessor medium-paint :initform nil
          :documentation "Persistent Impeller paint object for this medium.")
   (surface :accessor medium-surface :initform nil
            :documentation "The Impeller rendering surface."))
  (:documentation "McCLIM medium using Impeller for drawing."))

;;; Helper functions for medium drawing

(defun %get-medium-paint (medium)
  "Get or create the persistent paint object for this medium."
  (or (medium-paint medium)
      (setf (medium-paint medium) (frs:make-paint))))

(defun %get-medium-builder (medium)
  "Get the current display list builder from the port's delegate.
   Returns nil if not currently in a frame (shouldn't happen in normal use)."
  (let* ((port (port medium))
         (delegate (when port (port-delegate port))))
    (get-delegate-current-builder delegate)))

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
  "Draw a line from (x1, y1) to (x2, y2)."
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-ink medium))
         (builder (%get-medium-builder medium)))
    (when builder
      (with-ink-on-paint (paint ink medium)
        (configure-paint-for-stroke paint (medium-line-style medium) medium)
        (frs:with-path-builder (pb)
          (frs:path-move-to pb (float x1 1.0f0) (float y1 1.0f0))
          (frs:path-line-to pb (float x2 1.0f0) (float y2 1.0f0))
          (let ((path (frs:build-path pb)))
            (unwind-protect
                 (frs:draw-path builder path paint)
              (frs:release-path path))))))))

(defmethod medium-draw-lines* ((medium render-stack-medium) coord-seq)
  ;; Draw multiple lines
  (declare (ignore coord-seq)))

(defmethod medium-draw-polygon* ((medium render-stack-medium) coord-seq closed filled)
  "Draw a polygon given by coordinate sequence.

   coord-seq is a sequence of (x y) points: #(x0 y0 x1 y1 x2 y2 ...)
   closed: if T, close the path (connect last point to first)
   filled: if T, fill the polygon; otherwise stroke the outline"
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-ink medium))
         (builder (%get-medium-builder medium))
         (coords (coerce coord-seq 'vector)))
    (when (and builder (>= (length coords) 4))  ; Need at least 2 points (4 coordinates)
      (with-ink-on-paint (paint ink medium)
        (if filled
            (frs:paint-set-draw-style paint :fill)
            (configure-paint-for-stroke paint (medium-line-style medium) medium))
        (frs:with-path-builder (pb)
          ;; Move to first point
          (frs:path-move-to pb (float (aref coords 0) 1.0f0) (float (aref coords 1) 1.0f0))
          ;; Line to remaining points
          (loop for i from 2 below (length coords) by 2
                do (frs:path-line-to pb (float (aref coords i) 1.0f0) (float (aref coords (1+ i)) 1.0f0)))
          ;; Close path if requested
          (when closed
            (frs:path-close pb))
          ;; Build and draw the path
          (let ((path (frs:build-path pb)))
            (unwind-protect
                 (frs:draw-path builder path paint)
              (frs:release-path path))))))))

(defmethod medium-draw-rectangle* ((medium render-stack-medium) x1 y1 x2 y2 filled)
  "Draw a rectangle from (x1, y1) to (x2, y2).

   If filled is T, fills the rectangle with the medium's current ink.
   If filled is NIL, strokes the rectangle outline using the medium's line style."
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-ink medium))
         (builder (%get-medium-builder medium)))
    (when builder
      (with-ink-on-paint (paint ink medium)
        (if filled
            (frs:paint-set-draw-style paint :fill)
            (configure-paint-for-stroke paint (medium-line-style medium) medium))
        ;; Normalize rectangle coordinates (ensure positive width/height)
        (let ((nx (min x1 x2))
              (ny (min y1 y2))
              (nw (abs (- x2 x1)))
              (nh (abs (- y2 y1))))
          (frs:draw-rect builder
                         (float nx 1.0f0)
                         (float ny 1.0f0)
                         (float nw 1.0f0)
                         (float nh 1.0f0)
                         paint))))))

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
  "Ensure all drawing is complete.

   For render-stack, the render engine runs continuously and handles
   buffer swapping automatically. This method is a no-op in Phase 1.

   In Phase 4 with full McCLIM integration, this will coordinate with
   the render engine to ensure drawing commands are processed."
  ;; Phase 1: No-op - render engine handles frame presentation
  ;; Phase 4: Will signal render engine to present current frame
  )

(defmethod medium-force-output ((medium render-stack-medium))
  "Force any pending output to the display.

   For render-stack, drawing happens immediately when medium methods
   are called during the render delegate's draw callback.

   In Phase 4 with full McCLIM integration, this will trigger an
   immediate frame render if needed."
  ;; Phase 1: No-op - drawing is immediate within render delegate callback
  ;; Phase 4: Will trigger immediate frame presentation
  )

(defmethod medium-clear-area ((medium render-stack-medium) left top right bottom)
  "Clear a rectangular area using the medium's background color."
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-background medium))
         (builder (%get-medium-builder medium)))
    (when builder
      ;; Don't use with-ink-on-paint here since we know it's just a solid color
      (multiple-value-bind (r g b a)
          (clim-ink-to-impeller-color ink medium)
        (frs:paint-set-color paint r g b a)
        (frs:paint-set-draw-style paint :fill)
        ;; Normalize rectangle coordinates
        (let ((nx (min left right))
              (ny (min top bottom))
              (nw (abs (- right left)))
              (nh (abs (- bottom top))))
          (frs:draw-rect builder
                         (float nx 1.0f0)
                         (float ny 1.0f0)
                         (float nw 1.0f0)
                         (float nh 1.0f0)
                         paint))))))

;;; Font Metrics Protocol
;;; These methods provide text measurement for layout calculations

(defun %text-style-to-paragraph-metrics (text-style medium string)
  "Create a paragraph from text-style and string, return metrics.
   Returns (values height baseline width)."
  (let* ((port (port medium))
         (typo-ctx (port-typography-context port))
         (paint (%get-medium-paint medium))
         (style (frs:make-paragraph-style)))
    (unwind-protect
         (progn
           ;; Configure style from text-style
           (multiple-value-bind (family face size)
               (text-style-components text-style)
             (let ((mapped-family (case family
                                    (:serif "Roboto Serif")
                                    (:sans-serif "Roboto")
                                    (:fix "Roboto Mono")
                                    (t (or family "Roboto"))))
                   (mapped-size (case size
                                  (:tiny 8.0f0)
                                  (:very-small 10.0f0)
                                  (:small 12.0f0)
                                  (:normal 14.0f0)
                                  (:large 18.0f0)
                                  (:very-large 24.0f0)
                                  (:huge 32.0f0)
                                  (t (float (or size 14) 1.0f0))))
                   (weight (if (and (listp face) (member :bold face))
                               :bold
                               :normal)))
               (frs:paragraph-style-set-font-family style mapped-family)
               (frs:paragraph-style-set-font-size style mapped-size)
               (frs:paragraph-style-set-font-weight style weight)
               (frs:paragraph-style-set-foreground style paint)))
           ;; Build paragraph and get metrics
           (frs:with-paragraph-builder (builder typo-ctx)
             (frs:paragraph-builder-push-style builder style)
             (frs:paragraph-builder-add-text builder string)
             (frs:paragraph-builder-pop-style builder)
             (let ((paragraph (frs:paragraph-builder-build builder 10000.0f0)))
               (unwind-protect
                    (values (frs:paragraph-get-height paragraph)
                            (frs:paragraph-get-alphabetic-baseline paragraph)
                            (frs:paragraph-get-longest-line-width paragraph))
                 (frs:release-paragraph paragraph)))))
      (frs:release-paragraph-style style))))

(defmethod text-style-ascent ((text-style text-style) (medium render-stack-medium))
  "Return the ascent (distance above baseline) in pixels."
  (multiple-value-bind (height baseline width)
      (%text-style-to-paragraph-metrics text-style medium "M")
    (declare (ignore height width))
    (float baseline 1.0f0)))

(defmethod text-style-descent ((text-style text-style) (medium render-stack-medium))
  "Return the descent (distance below baseline) in pixels."
  (multiple-value-bind (height baseline width)
      (%text-style-to-paragraph-metrics text-style medium "M")
    (declare (ignore width))
    (float (- height baseline) 1.0f0)))

(defmethod text-style-height ((text-style text-style) (medium render-stack-medium))
  "Return the total height (ascent + descent) in pixels."
  (multiple-value-bind (height baseline width)
      (%text-style-to-paragraph-metrics text-style medium "M")
    (declare (ignore baseline width))
    (float height 1.0f0)))

(defmethod text-style-width ((text-style text-style) (medium render-stack-medium))
  "Return the width of the character 'M' in pixels."
  (multiple-value-bind (height baseline width)
      (%text-style-to-paragraph-metrics text-style medium "M")
    (declare (ignore height baseline))
    (float width 1.0f0)))

(defmethod text-size ((medium render-stack-medium) string
                      &key text-style (start 0) end)
  "Calculate the size of a text string.
   Returns five values: width, height, final-x, final-y, baseline."
  (let* ((text-style (or text-style (medium-text-style medium)))
         (substr (subseq string start (or end (length string))))
         (port (port medium))
         (typo-ctx (port-typography-context port))
         (paint (%get-medium-paint medium))
         (style (frs:make-paragraph-style)))
    (unwind-protect
         (progn
           ;; Configure style
           (multiple-value-bind (family face size)
               (text-style-components text-style)
             (let ((mapped-family (case family
                                    (:serif "Roboto Serif")
                                    (:sans-serif "Roboto")
                                    (:fix "Roboto Mono")
                                    (t (or family "Roboto"))))
                   (mapped-size (case size
                                  (:tiny 8.0f0)
                                  (:very-small 10.0f0)
                                  (:small 12.0f0)
                                  (:normal 14.0f0)
                                  (:large 18.0f0)
                                  (:very-large 24.0f0)
                                  (:huge 32.0f0)
                                  (t (float (or size 14) 1.0f0))))
                   (weight (if (and (listp face) (member :bold face))
                               :bold
                               :normal)))
               (frs:paragraph-style-set-font-family style mapped-family)
               (frs:paragraph-style-set-font-size style mapped-size)
               (frs:paragraph-style-set-font-weight style weight)
               (frs:paragraph-style-set-foreground style paint)))
           ;; Build paragraph and get metrics
           (frs:with-paragraph-builder (builder typo-ctx)
             (frs:paragraph-builder-push-style builder style)
             (frs:paragraph-builder-add-text builder substr)
             (frs:paragraph-builder-pop-style builder)
             (let ((paragraph (frs:paragraph-builder-build builder 10000.0f0)))
               (unwind-protect
                    (let ((width (frs:paragraph-get-longest-line-width paragraph))
                          (height (frs:paragraph-get-height paragraph))
                          (baseline (frs:paragraph-get-alphabetic-baseline paragraph)))
                      (values (float width 1.0f0)
                              (float height 1.0f0)
                              (float width 1.0f0)
                              0.0f0
                              (float baseline 1.0f0)))
                 (frs:release-paragraph paragraph)))))
      (frs:release-paragraph-style style))))

(defmethod medium-beep ((medium render-stack-medium))
  ;; Produce an audible beep
  )
