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
                 ;; Keep root as FlowTransformLayer until all children are added.
                 ;; Using the container-interface alias (root-container) with
                 ;; transform-layer-add-child causes UB -- use root directly.
                 (root  (frs:make-scale-transform-layer scale-x scale-y)))
            (frs:layer-tree-set-root-layer tree (frs:transform-layer-as-container root))
            ;; Add one DL subtree per pane.
            (let ((pane-count 0))
              (dolist (entry sorted)
                (let ((sheet (car entry))
                      (dl    (cdr entry)))
                  (multiple-value-bind (lx ly lw lh)
                      (%pane-logical-bounds sheet)
                    (log:info :render "build-pane-layer-tree: pane ~A bounds lx=~A ly=~A lw=~A lh=~A dl=~A"
                              (type-of sheet) lx ly lw lh (not (null dl)))
                    (when (and lx (plusp lw) (plusp lh))
                      (handler-case
                          (let* ((clip (frs:make-clip-rect-layer lx ly lw lh))
                                 (leaf (frs:make-display-list-layer dl)))
                            ;; leaf -> clip -> root.  Use root (FlowTransformLayer)
                            ;; directly for add-child, not the container-interface alias.
                            (frs:clip-rect-layer-add-child
                             clip (frs:display-list-layer-as-container leaf))
                            (frs:release-display-list-layer leaf)
                            (frs:transform-layer-add-child
                             root (frs:clip-rect-layer-as-container clip))
                            (frs:release-clip-rect-layer clip)
                            (incf pane-count))
                        (error (e)
                          (log:warn :render "build-pane-layer-tree: pane ~A: ~A"
                                    (type-of sheet) e)))))))
              (log:info :render "build-pane-layer-tree: added ~A panes, scale=~Ax~A phys=~Ax~A log=~Ax~A"
                        pane-count scale-x scale-y phys-w phys-h log-w log-h))
            ;; All children added. Tree retains root; release our reference.
            (frs:release-transform-layer root)
            tree)
        (error (e)
          (log:error :render "build-pane-layer-tree: ~A" e)
          nil)))))

(defun %composite-via-flow (runtime mirror snapshot surface &optional (damage-rects nil))
  "Rasterize SNAPSHOT pane DLs through the Flow compositor into a full-scene DL.

Builds a pane layer tree, acquires a scoped frame from the runtime's Flow
context, rasterizes the tree using frame damage for incremental GPU work,
and extracts a full-scene display list.

SURFACE is the FBO surface to draw to.  The scene DL is drawn to SURFACE
inside the scoped frame (the DL may reference frame-scoped state).

DAMAGE-RECTS is an optional list of (x y w h) physical-pixel rects from
mirror-take-damage-rects.  Combined with mirror-previous-layer-tree, Flow
uses these to compute a minimal re-rasterization region and reuse cached
layers for unchanged subtrees.

Retains the layer tree as MIRROR's previous-layer-tree for next frame's
damage diffing.

Returns the scene DL (caller must release) on success, or NIL on failure.
The DL has already been drawn to SURFACE before returning.

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
              ;; Pass null-pointer for Impeller context: the Flow compositor only
              ;; records layer paint ops into a DisplayListBuilder.  A real Impeller
              ;; context routes through PaintLayerTreeImpeller which produces empty
              ;; results; null routes through PaintLayerTreeSkia (recording path).
              ;;
              ;; IMPORTANT: surface-draw-display-list MUST be called INSIDE the
              ;; scoped frame.  FlowScopedFrameBuildDisplayListNew returns a DL
              ;; that references GPU resources (raster textures, render targets)
              ;; owned by the scoped frame.  Those resources are freed on
              ;; FlowScopedFrameRelease.  Drawing inside the frame keeps them valid.
              (frs:with-frame-damage (damage phys-w phys-h)
                ;; Wire in previous tree so Flow can diff for incremental rasterization.
                ;; mirror-previous-layer-tree and mirror-composite-deps are both still
                ;; alive at this point (released by render-delegate-draw after we return).
                (let ((prev-tree (mirror-previous-layer-tree mirror)))
                  (when prev-tree
                    (frs:frame-damage-set-previous-layer-tree damage prev-tree)))
                ;; Add physical-pixel damage rects from medium-finish-output.
                (dolist (rect damage-rects)
                  (destructuring-bind (x y w h) rect
                    (frs:frame-damage-add-additional-damage damage x y w h)))
                (frs:with-scoped-frame (frame flow-ctx (cffi:null-pointer) (cons phys-w phys-h))
                  (let ((status (frs:scoped-frame-raster frame tree
                                                         :frame-damage damage
                                                         :ignore-raster-cache t)))
                    (log:info :render "%composite-via-flow: raster status=~A phys=~Ax~A damage-rects=~A"
                              status phys-w phys-h (length damage-rects))
                    (when (eq status :success)
                      ;; Log the minimal clip rect Flow will actually re-rasterize.
                      (multiple-value-bind (has-damage dx dy dw dh)
                          (frs:frame-damage-compute-clip-rect damage tree)
                        (log:debug :render "%composite-via-flow: damage-clip has=~A rect=~A+~A ~Ax~A"
                                   has-damage dx dy dw dh))
                      (setf scene-dl (frs:scoped-frame-build-display-list frame))
                      (when (and scene-dl surface)
                        (let ((draw-ok (frs:surface-draw-display-list surface scene-dl)))
                          (log:info :render "%composite-via-flow: draw-result=~A" draw-ok))))))))
            (log:info :render "%composite-via-flow: scene-dl=~A drawn-to-surface=~A"
                      (not (null scene-dl)) (not (null surface)))
            ;; Retain tree as previous for next frame's damage diffing.
            ;; Release old previous-layer-tree AFTER rasterization is complete --
            ;; frame-damage-set-previous-layer-tree holds a reference during raster.
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
