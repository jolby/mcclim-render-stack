;;;; compositor.lisp - Flow layer tree construction for the per-pane composite
;;;;
;;;; Builds a Flow layer tree from the mirror's current pane-dl-map snapshot.
;;;;
;;;; Coordinate system:
;;;;   Pane DLs are recorded in window-absolute LOGICAL coords (the
;;;;   medium-device-transformation includes the pane's native offset).
;;;;   The layer tree frame size is in physical pixels.
;;;;   A root scale transform layer converts logical -> physical coords so
;;;;   all pane DLs compose correctly without per-pane translation.
;;;;
;;;; Per-pane structure (under root scale):
;;;;   clip-rect-layer(lx, ly, lw, lh)  -- logical window-absolute bounds
;;;;     display-list-layer(pane-dl)    -- leaf; DL drawn at window-absolute origin

(in-package :mcclim-render-stack)

(defun %pane-logical-bounds (sheet)
  "Return the logical window-absolute bounds (lx ly lw lh) of SHEET.
LX, LY are the pane's top-left corner in window space (logical pixels).
LW, LH are the pane's width and height (logical pixels).

Returns (values lx ly lw lh) as single-floats, or NIL if the sheet has
no region or the transform cannot be computed."
  (handler-case
      (multiple-value-bind (lx ly)
          (clim:transform-position (climi::sheet-native-transformation sheet) 0 0)
        (multiple-value-bind (rx1 ry1 rx2 ry2)
            (clim:bounding-rectangle* (clim:sheet-region sheet))
          (values (float lx 1.0f0)
                  (float ly 1.0f0)
                  (float (- rx2 rx1) 1.0f0)
                  (float (- ry2 ry1) 1.0f0))))
    (error (e)
      (log:warn :render "%pane-logical-bounds: sheet ~A error: ~A" (type-of sheet) e)
      (values nil nil nil nil))))

(defun build-pane-layer-tree (mirror snapshot)
  "Build a Flow layer tree from SNAPSHOT for MIRROR.

SNAPSHOT is an alist ((sheet . dl) ...) with each DL already retained by
the caller (via mirror-snapshot-pane-dls).  Panes are sorted by sheet depth
(shallowest first) for correct back-to-front compositing.

The layer tree uses a root scale transform to convert window-absolute logical
DL coordinates to physical pixel coordinates.  Each pane's DL is placed under
a clip-rect-layer clipped to the pane's logical bounds.

Returns a new ImpellerLayerTree (caller must release), or NIL if dims are
zero, snapshot is empty, or an error occurs.

Thread Contract: MUST be called on the main thread."
  (rs-internals:assert-main-thread build-pane-layer-tree)
  (when (null snapshot)
    (return-from build-pane-layer-tree nil))
  (let* ((phys-w (mirror-width  mirror))
         (phys-h (mirror-height mirror))
         (log-w  (mirror-logical-width  mirror))
         (log-h  (mirror-logical-height mirror)))
    (when (or (zerop phys-w) (zerop phys-h) (zerop log-w) (zerop log-h))
      (log:warn :render "build-pane-layer-tree: zero dims ~Ax~A / ~Ax~A, skipping"
                phys-w phys-h log-w log-h)
      (return-from build-pane-layer-tree nil))
    (let* ((scale-x (float (/ phys-w log-w) 1.0f0))
           (scale-y (float (/ phys-h log-h) 1.0f0))
           ;; Sort panes shallowest-first (parent panes behind child panes).
           (sorted  (sort (copy-list snapshot) #'<
                          :key (lambda (e) (%sheet-depth (car e))))))
      (handler-case
          (let* ((tree  (frs:make-layer-tree phys-w phys-h))
                 ;; Root scale layer: logical coords -> physical pixels.
                 (root  (frs:make-scale-transform-layer scale-x scale-y))
                 (root-container (frs:transform-layer-as-container root)))
            (frs:layer-tree-set-root-layer tree root-container)
            ;; Tree now retains root. Release our reference.
            (frs:release-transform-layer root)
            ;; Add one clip+DL subtree per pane.
            (dolist (entry sorted)
              (let ((sheet (car entry))
                    (dl    (cdr entry)))
                (multiple-value-bind (lx ly lw lh)
                    (%pane-logical-bounds sheet)
                  (when (and lx (plusp lw) (plusp lh))
                    (handler-case
                        (let* ((clip (frs:make-clip-rect-layer lx ly lw lh))
                               (leaf (frs:make-display-list-layer dl)))
                          ;; leaf -> clip -> root.  Each add-child retains;
                          ;; release our ref after handing off.
                          (frs:clip-rect-layer-add-child
                           clip (frs:display-list-layer-as-container leaf))
                          (frs:release-display-list-layer leaf)
                          (frs:transform-layer-add-child
                           root-container (frs:clip-rect-layer-as-container clip))
                          (frs:release-clip-rect-layer clip))
                      (error (e)
                        (log:warn :render "build-pane-layer-tree: pane ~A: ~A"
                                  (type-of sheet) e)))))))
            tree)
        (error (e)
          (log:error :render "build-pane-layer-tree: ~A" e)
          nil)))))

(defun %composite-via-flow (runtime mirror snapshot)
  "Rasterize SNAPSHOT pane DLs through the Flow compositor into a full-scene DL.

Builds a pane layer tree, acquires a scoped frame from the runtime's Flow
context, rasterizes the tree (using the raster cache for unchanged layers),
and extracts a full-scene display list.

Retains the layer tree as MIRROR's previous-layer-tree for Phase C damage
tracking.  SNAPSHOT DLs are not referenced by the returned DL and may be
released immediately after this call.

Returns a retained ImpellerDisplayList (caller must release), or NIL on
failure.

Thread Contract: MUST be called on the main thread."
  (rs-internals:assert-main-thread %composite-via-flow)
  (when (null snapshot)
    (return-from %composite-via-flow nil))
  (let* ((flow-ctx     (runtime-flow-context runtime))
         (impeller-ctx (runtime-impeller-context runtime))
         (phys-w       (mirror-width  mirror))
         (phys-h       (mirror-height mirror)))
    (unless (and flow-ctx impeller-ctx (plusp phys-w) (plusp phys-h))
      (log:warn :render "%composite-via-flow: missing context or zero dims")
      (return-from %composite-via-flow nil))
    (let ((tree (build-pane-layer-tree mirror snapshot)))
      (unless tree
        (return-from %composite-via-flow nil))
      (handler-case
          (let (scene-dl)
            (rs-internals:without-float-traps
              (frs:with-scoped-frame (frame flow-ctx impeller-ctx (cons phys-w phys-h))
                (let ((status (frs:scoped-frame-raster frame tree)))
                  (if (eq status :success)
                      (setf scene-dl (frs:scoped-frame-build-display-list frame))
                      (log:warn :render "%composite-via-flow: raster status ~A" status)))))
            ;; Retain tree as previous for next frame's damage diffing.
            (let ((old (mirror-previous-layer-tree mirror)))
              (when old (frs:release-layer-tree old)))
            (frs:retain-layer-tree tree)
            (setf (mirror-previous-layer-tree mirror) tree)
            (frs:release-layer-tree tree)
            scene-dl)
        (error (e)
          (log:error :render "%composite-via-flow: ~A" e)
          (frs:release-layer-tree tree)
          nil)))))
