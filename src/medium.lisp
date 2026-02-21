(in-package :mcclim-render-stack)

;;; Medium - the drawing surface abstraction

(defclass render-stack-medium (basic-medium)
  ((paint :accessor medium-paint :initform nil
          :documentation "Persistent Impeller paint object for this medium.")
   (surface :accessor medium-surface :initform nil
             :documentation "The Impeller rendering surface."))
  (:documentation "McCLIM medium using Impeller for drawing."))

(defmethod clim:make-medium ((port render-stack-port) sheet)
  "Create a render-stack-medium for the given port and sheet."
  (make-instance 'render-stack-medium :port port :sheet sheet))

;;; Helper functions for medium drawing

(defun %get-medium-paint (medium)
  "Get or create the persistent paint object for this medium."
  (or (medium-paint medium)
      (setf (medium-paint medium) (frs:make-paint))))

(defun %get-device-pixel-ratio (medium)
  "Get the device pixel ratio (HiDPI scale) from the medium's mirror."
  (let* ((sheet (clim:medium-sheet medium))
         (mirror (when sheet (climi::sheet-mirror sheet))))
    (if (and mirror (typep mirror 'render-stack-mirror))
        (let* ((pw (mirror-width mirror))
               (lw (mirror-logical-width mirror)))
          (if (and pw lw (> lw 0))
              (float (/ pw lw) 1.0f0)
              1.0f0))
        1.0f0)))

(defun %get-medium-builder (medium)
  "Get or create the Impeller display list builder for the current McCLIM frame.

Returns the builder stored on the medium's mirror, creating it if needed.
The builder is a persistent recording context: drawing ops accumulate into it
across multiple calls until medium-finish-output finalizes it into a display list.

Returns NIL if no render-stack-mirror is associated with this medium's sheet."
  (let* ((sheet (medium-sheet medium))
         (mirror (when sheet (climi::sheet-mirror sheet))))
    (unless (typep mirror 'render-stack-mirror)
      (log:warn :render "No render-stack-mirror: sheet=~A mirror-type=~A"
                (type-of sheet) (type-of mirror))
      (return-from %get-medium-builder nil))
    (or (mirror-display-list-builder mirror)
        ;; First drawing op for this frame — create a fresh builder.
        (let ((builder (frs:make-display-list-builder)))
          ;; Apply HiDPI scale so McCLIM logical coordinates map to physical pixels.
          ;; On 1x displays scale=1.0 (identity). On 2x, scale=2.0.
          (let* ((phys-w  (mirror-width mirror))
                 (phys-h  (mirror-height mirror))
                 (log-w   (mirror-logical-width mirror))
                 (log-h   (mirror-logical-height mirror))
                 (scale-x (if (and (plusp log-w) (plusp log-h))
                              (float (/ phys-w log-w) 1.0f0)
                              1.0f0))
                 (scale-y (if (and (plusp log-w) (plusp log-h))
                              (float (/ phys-h log-h) 1.0f0)
                              1.0f0)))
            (rs-internals:with-context-fields (:phys-w phys-w :phys-h phys-h
                                               :log-w log-w :log-h log-h
                                               :scale-x scale-x :scale-y scale-y)
              (log:debug :render "Creating DL builder"))
            (frs:display-list-builder-set-transform builder scale-x scale-y 0.0f0 0.0f0))
          (setf (mirror-display-list-builder mirror) builder)
          builder))))




;;; CLIM Internal Functions

(defclass rgba-color (clim:color)
  ((red :initarg :red :initform 0.0 :type (real 0 1) :accessor color-red)
   (green :initarg :green :initform 0.0 :type (real 0 1) :accessor color-green)
   (blue :initarg :blue :initform 0.0 :type (real 0 1) :accessor color-blue)
   (alpha :initarg :alpha :initform 1 :type (real 0 1) :accessor color-alpha)))

(defmethod clime:color-rgba ((color rgba-color))
  "Return the RGBA components of an rgba-color."
  (values (color-red color) (color-green color) (color-blue color) (color-alpha color)))

(defmethod clim:color-rgb ((color rgba-color))
  "Return the RGB components of an rgba-color."
  (values (color-red color) (color-green color) (color-blue color)))

(defun clim::%set-color-alpha (color alpha)
  "Set the alpha component of a color.
   For RGB colors, returns a new RGBA color with the specified alpha.
   For RGBA colors, modifies the alpha component in place.
   For other color types, returns the color unchanged."
   (typecase color
     (clim:color
      (multiple-value-bind (r g b) (clim:color-rgb color)
        (make-instance 'rgba-color
                       :red r
                       :green g
                       :blue b
                       :alpha alpha)))
    (rgba-color
     (setf (slot-value color 'alpha) alpha)
     color)
    (t color)))

(defun make-rgba-color (red green blue &optional (alpha 1.0))
  "Create a color with alpha channel support.
   Unlike make-rgb-color, this supports transparency."
  (make-instance 'rgba-color :red red :green green :blue blue :alpha alpha))

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
    ((typep ink 'rgba-color)
     (values (float (color-red ink) 1.0f0)
             (float (color-green ink) 1.0f0)
             (float (color-blue ink) 1.0f0)
             (float (color-alpha ink) 1.0f0)))
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

   Returns a color source if a gradient was used (caller must release it),
   or NIL for solid colors. Modifies paint in place."
  (let ((color-source (%make-color-source-from-gradient ink)))
    (if color-source
        (progn
          (frs:paint-set-color-source paint color-source)
          color-source)
        (progn
          (multiple-value-bind (r g b a)
              (clim-ink-to-impeller-color ink medium)
            (frs:paint-set-color paint r g b a))
          nil))))

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

(defun apply-line-style-dashes (paint line-style)
  "Apply dash pattern from line-style to paint. Not yet implemented."
  (declare (ignore paint line-style))
  nil)

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
          (t :miter))))
    ;; Apply dash pattern (placeholder - not yet implemented)
    (apply-line-style-dashes paint line-style)))

;;; Device transformation helpers

(defun %get-medium-transformation (medium)
  "Get the device transformation for the medium.
   Returns the transformation mapping medium coordinates to device (window) coordinates."
  (climi::medium-device-transformation medium))

(defun %rectilinear-p (transformation)
  "Check if TRANSFORMATION is rectilinear (axis-aligned, no rotation/shear)."
  (rectilinear-transformation-p transformation))

(defun %compute-transform-scale (medium transformation)
  "Estimate the uniform scaling factor from TRANSFORMATION.
   Used to scale point sizes so they remain visible under transformations."
  (declare (ignore medium))
  (if (identity-transformation-p transformation)
      1.0f0
      (multiple-value-bind (x y)
          (transform-distance transformation 1.0 0.0)
        (float (sqrt (+ (* x x) (* y y))) 1.0f0))))

(defmacro %with-normalized-rect ((nx ny nw nh left top right bottom) &body body)
  "Bind NX, NY, NW, NH to normalized rectangle coordinates (positive width/height)."
  `(let* ((,nx (min ,left ,right))
          (,ny (min ,top ,bottom))
          (,nw (abs (- ,right ,left)))
          (,nh (abs (- ,bottom ,top))))
     ,@body))

;;; Medium drawing operations

(defmethod medium-draw-point* ((medium render-stack-medium) x y)
  "Draw a single point at (x, y) as a small filled rectangle."
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-ink medium))
         (tr (%get-medium-transformation medium))
         (builder (%get-medium-builder medium)))
    (when builder
      (with-ink-on-paint (paint ink medium)
        (let ((scale (%compute-transform-scale medium tr)))
          (frs:paint-set-draw-style paint :fill)
          (climi::with-transformed-position (tr x y)
            (frs:draw-rect builder
                           (float x 1.0f0)
                           (float y 1.0f0)
                           scale scale
                           paint)))))))

(defmethod medium-draw-points* ((medium render-stack-medium) coord-seq)
  "Draw multiple points from coord-seq (sequence of x y pairs)."
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-ink medium))
         (tr (%get-medium-transformation medium))
         (builder (%get-medium-builder medium))
         (coords (coerce coord-seq 'vector)))
    (when (and builder (>= (length coords) 2))
      (with-ink-on-paint (paint ink medium)
        (let ((scale (%compute-transform-scale medium tr)))
          (frs:paint-set-draw-style paint :fill)
          (loop for i from 0 below (length coords) by 2
                do (let ((x (aref coords i))
                         (y (aref coords (1+ i))))
                     (climi::with-transformed-position (tr x y)
                       (frs:draw-rect builder
                                      (float x 1.0f0)
                                      (float y 1.0f0)
                                      scale scale
                                      paint)))))))))

(defmethod medium-draw-line* ((medium render-stack-medium) x1 y1 x2 y2)
  "Draw a line from (x1, y1) to (x2, y2)."
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-ink medium))
         (tr (%get-medium-transformation medium))
         (builder (%get-medium-builder medium)))
    (when builder
      (with-ink-on-paint (paint ink medium)
        (configure-paint-for-stroke paint (medium-line-style medium) medium)
        (climi::with-transformed-positions* (tr x1 y1 x2 y2)
          (frs:with-path-builder (pb)
            (frs:path-move-to pb (float x1 1.0f0) (float y1 1.0f0))
            (frs:path-line-to pb (float x2 1.0f0) (float y2 1.0f0))
            (let ((path (frs:build-path pb)))
              (unwind-protect
                   (frs:draw-path builder path paint)
                (frs:release-path path)))))))))

(defmethod medium-draw-lines* ((medium render-stack-medium) coord-seq)
  "Draw multiple independent line segments from coord-seq.
   coord-seq contains groups of 4 values: x1 y1 x2 y2 for each line segment."
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-ink medium))
         (tr (%get-medium-transformation medium))
         (builder (%get-medium-builder medium))
         (coords (coerce coord-seq 'vector)))
    (when (and builder (>= (length coords) 4))
      (with-ink-on-paint (paint ink medium)
        (configure-paint-for-stroke paint (medium-line-style medium) medium)
        (frs:with-path-builder (pb)
          (loop for i from 0 below (length coords) by 4
                when (<= (+ i 3) (length coords))
                do (let ((x1 (aref coords i))
                         (y1 (aref coords (+ i 1)))
                         (x2 (aref coords (+ i 2)))
                         (y2 (aref coords (+ i 3))))
                     (climi::with-transformed-positions* (tr x1 y1 x2 y2)
                       (frs:path-move-to pb (float x1 1.0f0) (float y1 1.0f0))
                       (frs:path-line-to pb (float x2 1.0f0) (float y2 1.0f0)))))
          (let ((path (frs:build-path pb)))
            (unwind-protect
                 (frs:draw-path builder path paint)
              (frs:release-path path))))))))

(defmethod medium-draw-polygon* ((medium render-stack-medium) coord-seq closed filled)
  "Draw a polygon given by coordinate sequence.

   coord-seq is a sequence of (x y) points: #(x0 y0 x1 y1 x2 y2 ...)
   closed: if T, close the path (connect last point to first)
   filled: if T, fill the polygon; otherwise stroke the outline"
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-ink medium))
         (tr (%get-medium-transformation medium))
         (builder (%get-medium-builder medium))
         (coords (coerce coord-seq 'vector)))
    (when (and builder (>= (length coords) 4))
      (with-ink-on-paint (paint ink medium)
        (if filled
            (frs:paint-set-draw-style paint :fill)
            (configure-paint-for-stroke paint (medium-line-style medium) medium))
        (frs:with-path-builder (pb)
          ;; Move to first point (transformed)
          (multiple-value-bind (x y)
              (transform-position tr (aref coords 0) (aref coords 1))
            (frs:path-move-to pb (float x 1.0f0) (float y 1.0f0)))
          ;; Line to remaining points (transformed)
          (loop for i from 2 below (length coords) by 2
                do (multiple-value-bind (x y)
                       (transform-position tr (aref coords i) (aref coords (1+ i)))
                     (frs:path-line-to pb (float x 1.0f0) (float y 1.0f0))))
          (when closed
            (frs:path-close pb))
          (let ((path (frs:build-path pb)))
            (unwind-protect
                 (frs:draw-path builder path paint)
              (frs:release-path path))))))))

(defmethod medium-draw-rectangle* ((medium render-stack-medium) x1 y1 x2 y2 filled)
  "Draw a rectangle from (x1, y1) to (x2, y2).

   If filled is T, fills the rectangle with the medium's current ink.
   If filled is NIL, strokes the rectangle outline using the medium's line style.
   Uses optimized draw-rect for axis-aligned transforms, falls back to
   path drawing for rotated/sheared rectangles."
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-ink medium))
         (tr (%get-medium-transformation medium))
         (builder (%get-medium-builder medium)))
    (when builder
      (with-ink-on-paint (paint ink medium)
        (if filled
            (frs:paint-set-draw-style paint :fill)
            (configure-paint-for-stroke paint (medium-line-style medium) medium))
        (if (%rectilinear-p tr)
            ;; Fast path: axis-aligned transform — use draw-rect
            (climi::with-transformed-positions* (tr x1 y1 x2 y2)
              (%with-normalized-rect (nx ny nw nh x1 y1 x2 y2)
                (frs:draw-rect builder
                               (float nx 1.0f0) (float ny 1.0f0)
                               (float nw 1.0f0) (float nh 1.0f0)
                               paint)))
            ;; General path: rotated/sheared — transform each corner
            (frs:with-path-builder (pb)
              (multiple-value-bind (ax ay) (transform-position tr x1 y1)
                (frs:path-move-to pb (float ax 1.0f0) (float ay 1.0f0)))
              (multiple-value-bind (bx by) (transform-position tr x2 y1)
                (frs:path-line-to pb (float bx 1.0f0) (float by 1.0f0)))
              (multiple-value-bind (cx cy) (transform-position tr x2 y2)
                (frs:path-line-to pb (float cx 1.0f0) (float cy 1.0f0)))
              (multiple-value-bind (dx dy) (transform-position tr x1 y2)
                (frs:path-line-to pb (float dx 1.0f0) (float dy 1.0f0)))
              (frs:path-close pb)
              (let ((path (frs:build-path pb)))
                (unwind-protect
                     (frs:draw-path builder path paint)
                  (frs:release-path path)))))))))

(defmethod medium-draw-ellipse* ((medium render-stack-medium)
                                  center-x center-y
                                  radius-1-dx radius-1-dy
                                  radius-2-dx radius-2-dy
                                  start-angle end-angle filled)
  "Draw an ellipse centered at (center-x, center-y).

   radius-1-dx/dy and radius-2-dx/dy define the two radius vectors.
   For a circle: radius-1-dx = r, radius-1-dy = 0, radius-2-dx = 0, radius-2-dy = r
   start-angle and end-angle are in radians (nil means full ellipse).
   filled: T to fill, NIL to stroke."
  (declare (ignore start-angle end-angle))
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-ink medium))
         (tr (%get-medium-transformation medium))
         (builder (%get-medium-builder medium)))
    (when builder
      (with-ink-on-paint (paint ink medium)
        (if filled
            (frs:paint-set-draw-style paint :fill)
            (configure-paint-for-stroke paint (medium-line-style medium) medium))
        ;; Transform center and radius vectors
        (climi::with-transformed-position (tr center-x center-y)
          (climi::with-transformed-distance (tr radius-1-dx radius-1-dy)
            (climi::with-transformed-distance (tr radius-2-dx radius-2-dy)
              (let* ((radius-x (max (abs radius-1-dx) (abs radius-2-dx)))
                     (radius-y (max (abs radius-1-dy) (abs radius-2-dy)))
                     (left (- center-x radius-x))
                     (top (- center-y radius-y))
                     (right (+ center-x radius-x))
                     (bottom (+ center-y radius-y)))
                (%with-normalized-rect (nx ny nw nh left top right bottom)
                  (frs:draw-oval builder
                                 (float nx 1.0f0) (float ny 1.0f0)
                                 (float nw 1.0f0) (float nh 1.0f0)
                                 paint))))))))))

(defmethod medium-draw-text* ((medium render-stack-medium)
                                string x y
                                start end
                                align-x align-y
                                toward-x toward-y transform-glyphs)
  "Draw text at position (x, y) with alignment.

   align-x: :left, :center, :right
   align-y: :top, :center, :baseline, :bottom

   toward-x/y and transform-glyphs are ignored in Phase 2."
   (declare (ignore toward-x toward-y transform-glyphs))
   (let* ((builder (%get-medium-builder medium))
          (tr (%get-medium-transformation medium))
          (port (port medium)))
     (when (and builder
                (> (- (or end (length string)) (or start 0)) 0))
       (climi::with-transformed-position (tr x y)
         (let* ((text-style (medium-text-style medium))
                (substr (subseq string (or start 0) (or end (length string))))
                (typo-ctx (runtime-typography-context (port-runtime port)))
                (paint (%get-medium-paint medium))
                (style (frs:make-paragraph-style)))
           (unwind-protect
                (progn
                  ;; Configure text style
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
                                      :weight700
                                      :weight400))
                          (slant (if (and (listp face) (member :italic face))
                                     :italic
                                     :normal)))
                      (frs:paragraph-style-set-font-family style mapped-family)
                      (frs:paragraph-style-set-font-size style mapped-size)
                      (frs:paragraph-style-set-font-weight style weight)
                      (frs:paragraph-style-set-font-style style slant)
                      ;; Set text color from medium ink
                      (multiple-value-bind (r g b a)
                          (clim-ink-to-impeller-color (medium-ink medium) medium)
                        (frs:paint-set-color paint r g b a)
                        (frs:paragraph-style-set-foreground style paint))))
                  ;; Build paragraph
                  (frs:with-paragraph-builder (pb typo-ctx)
                    (frs:paragraph-builder-push-style pb style)
                    (frs:paragraph-builder-add-text pb substr)
                    (frs:paragraph-builder-pop-style pb)
                    (let ((paragraph (frs:paragraph-builder-build pb 10000.0f0)))
                      (unwind-protect
                           (let* ((width (frs:paragraph-get-longest-line-width paragraph))
                                  (height (frs:paragraph-get-height paragraph))
                                  (baseline (frs:paragraph-get-alphabetic-baseline paragraph))
                                  (draw-x (float (case align-x
                                                   ((:left nil) x)
                                                   (:center (- x (/ width 2.0)))
                                                   (:right (- x width))
                                                   (t x))
                                                 1.0f0))
                                  (draw-y (float (case align-y
                                                   ((:top nil) y)
                                                   (:center (- y (/ height 2.0)))
                                                   (:baseline (- y baseline))
                                                   (:bottom (- y height))
                                                   (t y))
                                                 1.0f0)))
                             (frs:draw-paragraph builder paragraph draw-x draw-y))
                        (frs:release-paragraph paragraph)))))
             (frs:release-paragraph-style style)))))))

;;; Medium state

(defmethod climi::invoke-with-output-buffered
    ((medium render-stack-medium) continuation &optional (buffered-p t))
  "Wrap the repaint continuation with Impeller DL lifecycle.

McCLIM calls this (via the output-recording-stream trampoline) to wrap
display/repaint operations. The unwind-protect ensures medium-finish-output
is always called after drawing ops accumulate in the builder, publishing
the DL to the mirror for the main thread to rasterize.

Thread Contract: Called on UI thread."
  (let ((was-buffering (medium-buffering-output-p medium)))
    (cond
      ((and buffered-p (not was-buffering))
       ;; Top-level buffered entry: run ops, then finalize DL.
       (setf (medium-buffering-output-p medium) t)
       (unwind-protect
            (funcall continuation)
         (medium-finish-output medium)
         (setf (medium-buffering-output-p medium) nil)))
      ((and (not buffered-p) was-buffering)
       ;; Forced flush: finalize current DL, then run continuation unbuffered.
       (medium-finish-output medium)
       (setf (medium-buffering-output-p medium) nil)
       (funcall continuation))
      (t
       ;; Nested call or no change in buffering state.
       (funcall continuation)))))

(defmethod medium-finish-output ((medium render-stack-medium))
  "Finalize the current frame's display list and publish it for rasterization.

Called by McCLIM after the display function completes (e.g., via finish-output
on the pane, or by invoke-with-output-buffered's unwind-protect).

Only finalizes and publishes from the top-level mirrored sheet.  Sub-pane
mediums accumulate into the shared mirror builder but are no-ops here —
the builder accumulates all pane content and is finalized once by the
top-level sheet (identified by having a direct mirror).

Thread Contract: Called on UI thread."
  (let* ((sheet  (medium-sheet medium))
         (mirror (when sheet (climi::sheet-mirror sheet))))
    (log:trace :render "medium-finish-output: sheet=~A mirror-type=~A"
               (type-of sheet) (type-of mirror))
    (when (typep mirror 'render-stack-mirror)
      ;; Only finalize/publish from the top-level mirrored sheet.
      ;; Sub-pane mediums draw into the shared builder but don't finalize it.
      (unless (climi::sheet-direct-mirror sheet)
        (return-from medium-finish-output nil))
      (let ((builder (mirror-display-list-builder mirror)))
        (log:debug :render "medium-finish-output: builder=~A" builder)
        (when builder
          (handler-case
              (let ((dl (frs:create-display-list builder)))
                ;; Builder is spent — release it and clear the slot.
                (frs:release-display-list-builder builder)
                (setf (mirror-display-list-builder mirror) nil)
                ;; Publish DL for the main thread.  Drops any unconsumed
                ;; previous DL (mirror-store-pending-dl releases it).
                (log:debug :render "Published DL ~A" dl)
                (mirror-store-pending-dl mirror dl))
            (error (e)
              (log:error :render "medium-finish-output error: ~A" e)
              ;; Avoid leaking the builder on error.
              (frs:release-display-list-builder builder)
              (setf (mirror-display-list-builder mirror) nil))))))))

(defmethod medium-force-output ((medium render-stack-medium))
  "Force any pending output to the display — no-op for render-stack.
The render loop on the main thread handles presentation."
  )


(defmethod medium-clear-area ((medium render-stack-medium) left top right bottom)
  "Clear a rectangular area using the medium's background color."
  (let* ((paint (%get-medium-paint medium))
         (ink (medium-background medium))
         (tr (%get-medium-transformation medium))
         (builder (%get-medium-builder medium)))
    (when builder
      (multiple-value-bind (r g b a)
          (clim-ink-to-impeller-color ink medium)
        (frs:paint-set-color paint r g b a)
        (frs:paint-set-draw-style paint :fill)
        (climi::with-transformed-positions* (tr left top right bottom)
          (%with-normalized-rect (nx ny nw nh left top right bottom)
            (frs:draw-rect builder
                           (float nx 1.0f0)
                           (float ny 1.0f0)
                           (float nw 1.0f0)
                           (float nh 1.0f0)
                           paint)))))))

;;; Font Metrics Protocol
;;; These methods provide text measurement for layout calculations

(defun %text-style-to-paragraph-metrics (text-style medium string)
   "Create a paragraph from text-style and string, return metrics.
    Returns (values height baseline width)."
   (let* ((port (port medium))
          (typo-ctx (runtime-typography-context (port-runtime port)))
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
                               :weight700
                               :weight400)))
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

(defmethod clim-backend:text-bounding-rectangle*
    ((medium render-stack-medium) string &key text-style (start 0) end)
  "Calculate bounding rectangle for text string.
   Returns six values: left, top, right, bottom, cursor-dx, cursor-dy."
  (multiple-value-bind (width height final-x final-y baseline)
      (text-size medium string :text-style text-style :start start :end end)
    (declare (ignore final-x final-y baseline))
    (if (zerop (length string))
        (values 0.0f0 0.0f0 0.0f0 (float height 1.0f0) 0.0f0 0.0f0)
        (values 0.0f0 0.0f0 (float width 1.0f0) (float height 1.0f0)
                (float width 1.0f0) 0.0f0))))

(defmethod text-size ((medium render-stack-medium) string
                      &key text-style (start 0) end)
  "Calculate the size of a text string.
   Returns five values: width, height, final-x, final-y, baseline."
  (let* ((text-style (or text-style (medium-text-style medium)))
         (substr (subseq string start (or end (length string))))
         (port (port medium))
         (typo-ctx (runtime-typography-context (port-runtime port)))
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
                               :weight700
                               :weight400)))
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
