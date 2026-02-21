;;;; example/elevated-card-demo.lisp
;;;; Visual demo of Material Design elevated cards with drop shadows

(defpackage :impeller-elevated-card-demo
  (:use :clim :clim-lisp)
  (:export #:run))

(in-package :impeller-elevated-card-demo)

;;; Demo application showing Material Design elevation shadows
;;;
;;; This demonstrates Impeller's native shadow API which computes shadows
;;; from Material Design elevation values (dp). Higher elevation = larger,
;;; softer shadows.

(define-application-frame elevated-card-demo ()
  ()
  (:panes
   (canvas :application
           :display-function 'display-canvas
           :scroll-bars nil
           :background (make-rgb-color 0.95 0.95 0.97)))  ; Light gray background
  (:layouts
   (default
    (vertically ()
      (outlining (:thickness 2)
        (labelling (:label "Elevated Card Demo - Material Design Shadows")
          (outlining (:thickness 10)
            canvas)))))))

(defun draw-elevated-card (stream x y width height
                           &key (elevation 4)
                                (corner-radius 8)
                                (background +white+)
                                (title nil)
                                (subtitle nil))
  "Draw a Material Design elevated card with shadow.

Arguments:
   stream        - CLIM output stream
   x, y          - Top-left corner position
   width, height - Card dimensions
   elevation     - Material Design elevation in dp (affects shadow size)
   corner-radius - Corner radius for rounded rectangle
   background    - Card background color
   title         - Optional title text
   subtitle      - Optional subtitle text

Uses the recorded shadow API (draw-elevated-shadow*) so shadows persist
across output recording and replay (e.g., window resize)."
  ;; Draw the shadow FIRST (painter's algorithm - shadow behind card)
  ;; This records the shadow in the output history
  (mcclim-sdl3-impeller-backend:draw-elevated-shadow*
   stream
   x y (+ x width) (+ y height)
   :elevation elevation
   :corner-radius corner-radius
   :shadow-color +black+
   :shadow-alpha 0.25)

  ;; Draw the card background as a rounded rectangle
  ;; Uses the recorded API so it survives output recording/replay
  (mcclim-sdl3-impeller-backend:draw-rounded-rectangle*
   stream
   x y (+ x width) (+ y height)
   :radius corner-radius
   :filled t
   :ink background)

  ;; Draw text on top of the card
  (when title
    (draw-text* stream title
                (+ x 12) (+ y 12)
                :align-y :top
                :text-style (make-text-style :sans-serif :bold :normal)))
  (when subtitle
    (draw-text* stream subtitle
                (+ x 12) (+ y 32)
                :align-y :top
                :text-style (make-text-style :sans-serif :roman :small)
                :ink (make-rgb-color 0.4 0.4 0.4))))

(defun display-canvas (frame stream)
  (declare (ignore frame))
  (let ((x 40) (y 30))

    ;; Title
    (draw-text* stream "Material Design Elevation Shadows" x y
                :align-y :top
                :text-style (make-text-style :sans-serif :bold :very-large))
    (incf y 50)

    (draw-text* stream "Cards at different elevation levels (higher = larger shadow):" x y
                :align-y :top
                :text-style (make-text-style :sans-serif :roman :normal)
                :ink (make-rgb-color 0.3 0.3 0.3))
    (incf y 65)

    ;; Row 1: Basic elevation levels
    (let ((card-width 180)
          (card-height 120)
          (spacing 220))

      ;; Elevation 1dp - Subtle shadow
      (draw-elevated-card stream x y card-width card-height
                          :elevation 1
                          :title "Elevation: 1dp"
                          :subtitle "Subtle lift")

      ;; Elevation 2dp - Light shadow
      (draw-elevated-card stream (+ x spacing) y card-width card-height
                          :elevation 2
                          :title "Elevation: 2dp"
                          :subtitle "Slight shadow")

      ;; Elevation 4dp - Medium shadow
      (draw-elevated-card stream (+ x (* 2 spacing)) y card-width card-height
                          :elevation 4
                          :title "Elevation: 4dp"
                          :subtitle "Standard card")

      ;; Elevation 8dp - Prominent shadow
      (draw-elevated-card stream (+ x (* 3 spacing)) y card-width card-height
                          :elevation 8
                          :title "Elevation: 8dp"
                          :subtitle "Raised element"))

      (incf y 200)

    ;; Row 2: Higher elevations
    (let ((card-width 200)
          (card-height 120)
          (spacing 240))

      ;; Elevation 12dp
      (draw-elevated-card stream x y card-width card-height
                          :elevation 12
                          :title "Elevation: 12dp"
                          :subtitle "Floating action")

      ;; Elevation 16dp
      (draw-elevated-card stream (+ x spacing) y card-width card-height
                          :elevation 16
                          :title "Elevation: 16dp"
                          :subtitle "Nav drawer")

      ;; Elevation 24dp - Highest
      (draw-elevated-card stream (+ x (* 2 spacing)) y card-width card-height
                          :elevation 24
                          :title "Elevation: 24dp"
                          :subtitle "Dialog/Modal"))

      (incf y 200)

    ;; Row 3: Different corner radii
    (draw-text* stream "Corner radius variations (all at 8dp elevation):" x y
                :align-y :top
                :text-style (make-text-style :sans-serif :roman :normal)
                :ink (make-rgb-color 0.3 0.3 0.3))
    (incf y 65)

    (let ((card-width 160)
          (card-height 100)
          (spacing 180))

      ;; Sharp corners
      (draw-elevated-card stream x y card-width card-height
                          :elevation 8
                          :corner-radius 0
                          :title "radius: 0"
                          :subtitle "Sharp corners")

      ;; Small radius
      (draw-elevated-card stream (+ x spacing) y card-width card-height
                          :elevation 8
                          :corner-radius 4
                          :title "radius: 4"
                          :subtitle "Slight rounding")

      ;; Medium radius
      (draw-elevated-card stream (+ x (* 2 spacing)) y card-width card-height
                          :elevation 8
                          :corner-radius 12
                          :title "radius: 12"
                          :subtitle "Softer corners")

      ;; Large radius
      (draw-elevated-card stream (+ x (* 3 spacing)) y card-width card-height
                          :elevation 8
                          :corner-radius 24
                          :title "radius: 24"
                          :subtitle "Pill-like")

      ;; Extra large radius
      (draw-elevated-card stream (+ x (* 4 spacing)) y card-width card-height
                          :elevation 8
                          :corner-radius 40
                          :title "radius: 40"
                          :subtitle "Very rounded"))

      (incf y 180)

    ;; Row 4: Colored cards
    (draw-text* stream "Colored cards with shadows:" x y
                :align-y :top
                :text-style (make-text-style :sans-serif :roman :normal)
                :ink (make-rgb-color 0.3 0.3 0.3))
    (incf y 65)

    (let ((card-width 160)
          (card-height 100)
          (spacing 180))

      ;; Light blue card
      (draw-elevated-card stream x y card-width card-height
                          :elevation 6
                          :corner-radius 8
                          :background (make-rgb-color 0.88 0.93 1.0)
                          :title "Info Card"
                          :subtitle "Light blue tint")

      ;; Light green card
      (draw-elevated-card stream (+ x spacing) y card-width card-height
                          :elevation 6
                          :corner-radius 8
                          :background (make-rgb-color 0.88 0.97 0.88)
                          :title "Success"
                          :subtitle "Light green")

      ;; Light yellow card
      (draw-elevated-card stream (+ x (* 2 spacing)) y card-width card-height
                          :elevation 6
                          :corner-radius 8
                          :background (make-rgb-color 1.0 0.98 0.88)
                          :title "Warning"
                          :subtitle "Light amber")

      ;; Light red card
      (draw-elevated-card stream (+ x (* 3 spacing)) y card-width card-height
                          :elevation 6
                          :corner-radius 8
                          :background (make-rgb-color 1.0 0.92 0.90)
                          :title "Error"
                          :subtitle "Light red tint"))

    (incf y 140)

    ;; Footer note
    (draw-text* stream "Press Q or Escape to exit" x y
                :align-y :top
                :text-style (make-text-style :sans-serif :italic :small)
                :ink (make-rgb-color 0.5 0.5 0.5))

    (finish-output stream)))

(defmethod handle-event ((pane application-pane) (event key-press-event))
  (let ((frame (pane-frame pane))
        (key-name (keyboard-event-key-name event)))
    (when (member key-name '(:escape :q))
      (frame-exit frame))))

(defun run ()
  "Run the elevated card demo using the sdl3/impeller backend.
   
   Window positioning demo: no :left/:top specified, so window auto-centers
   on the primary display."
  (setf clim:*default-server-path* '(:sdl3-impeller))
  (run-frame-top-level
   (make-application-frame 'elevated-card-demo
                           :width 1000
                           :height 700)))
