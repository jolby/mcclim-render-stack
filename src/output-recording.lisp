;;;; output-recording.lisp — Impeller-specific output recording and caching
;;;;
;;;; Provides a cached output mixin that stores an Impeller DisplayList for
;;;; each output record, avoiding re-recording when the record hasn't changed.
;;;; Also provides dirty tracking and invalidation hooks for the standard
;;;; McCLIM output record protocol.

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; Cached Output Mixin
;;; ============================================================================

(defclass render-stack-cached-output-mixin ()
  ((display-list
    :initform nil
    :accessor cached-display-list
    :documentation "Cached Impeller DisplayList pointer. Owned by this record.")
   (needs-rebuild
    :initform t
    :accessor cached-needs-rebuild
    :documentation "Dirty flag. T if the DisplayList needs to be rebuilt.")
   (bounds-when-cached
    :initform nil
    :accessor cached-bounds-when-cached
    :documentation "Bounding rectangle when the DisplayList was last built.")
   (dl-generation
    :initform 0
    :accessor cached-dl-generation
    :documentation "Generation counter for debugging/cache verification."))
  (:documentation
   "Mixin for output records that maintain a cached Impeller DisplayList.
    Implements dirty tracking via mark-needs-rebuild and standard CLIM hooks."))

;;; ============================================================================
;;; Dirty Tracking Protocol
;;; ============================================================================

(defgeneric mark-needs-rebuild (record)
  (:documentation
   "Mark RECORD as needing its DisplayList rebuilt.
    Releases the old DisplayList and propagates dirty state to the parent."))

(defmethod mark-needs-rebuild ((record t))
  (let ((parent (output-record-parent record)))
    (when parent
      (mark-needs-rebuild parent))))

(defmethod mark-needs-rebuild ((record render-stack-cached-output-mixin))
  (unless (cached-needs-rebuild record)
    (setf (cached-needs-rebuild record) t)
    ;; Release old display list to free resources
    (%release-record-display-list record)
    ;; Propagate to parent output record
    (let ((parent (output-record-parent record)))
      (when parent
        (mark-needs-rebuild parent)))))

(defun %release-record-display-list (record)
  "Release the cached display list for RECORD, if any."
  (let ((dl (cached-display-list record)))
    (when dl
      (frs:release-display-list dl)
      (setf (cached-display-list record) nil))))

;;; ============================================================================
;;; Invalidation Hooks — McCLIM notifies us when records change
;;; ============================================================================

(defmethod (setf output-record-position) :after
    (nx ny (record render-stack-cached-output-mixin))
  (declare (ignore nx ny))
  (mark-needs-rebuild record))

(defmethod (setf climi::.output-record-position-star.) :after
    (nx ny (record render-stack-cached-output-mixin))
  (declare (ignore nx ny))
  (mark-needs-rebuild record))

(defmethod add-output-record :after
    (child (record render-stack-cached-output-mixin))
  (declare (ignore child))
  (mark-needs-rebuild record))

(defmethod delete-output-record :after
    (child (record render-stack-cached-output-mixin) &optional errorp)
  (declare (ignore child errorp))
  (mark-needs-rebuild record))

(defmethod climi::clear-output-record :after
    ((record render-stack-cached-output-mixin))
  (mark-needs-rebuild record))

(defmethod (setf climi::output-record-dirty) :after
    (new-value (record render-stack-cached-output-mixin))
  (when (member new-value '(:updating :updated :moved))
    (mark-needs-rebuild record)))

;;; Cleanup — release DL when record loses its sheet

(defmethod climi::note-output-record-lost-sheet :after
    ((record render-stack-cached-output-mixin) sheet)
  (declare (ignore sheet))
  (%release-record-display-list record))

;;; ============================================================================
;;; Updating Output Record
;;; ============================================================================

(defclass render-stack-updating-output-record
    (standard-updating-output-record render-stack-cached-output-mixin)
  ()
  (:documentation
   "Specialized updating output record that caches an Impeller DisplayList."))

;;; ============================================================================
;;; Replay Logic — use cached DisplayList when available
;;; ============================================================================

(defun rebuild-cached-display-list (record stream)
  "Rebuild the cached DisplayList for RECORD by replaying its children."
  (%release-record-display-list record)
  (frs:with-display-list-builder (builder)
    (let ((medium (sheet-medium stream)))
      ;; Temporarily redirect drawing into our builder
      (let ((old-builder (mirror-display-list-builder
                          (climi::sheet-mirror (medium-sheet medium))))
            (old-buffering (medium-buffering-output-p medium)))
        ;; Store builder on mirror so %get-medium-builder finds it
        (setf (mirror-display-list-builder
               (climi::sheet-mirror (medium-sheet medium))) builder)
        (setf (medium-buffering-output-p medium) t)
        (unwind-protect
             (climi::replay-output-record-list record stream)
          (setf (mirror-display-list-builder
                 (climi::sheet-mirror (medium-sheet medium))) old-builder)
          (setf (medium-buffering-output-p medium) old-buffering))))
    ;; Capture the display list
    (let ((dl (frs:create-display-list builder)))
      (setf (cached-display-list record) dl
            (cached-needs-rebuild record) nil
            (cached-bounds-when-cached record) (bounding-rectangle record))
      (incf (cached-dl-generation record)))))

(defmethod replay-output-record :around
    ((record render-stack-cached-output-mixin) stream
     &optional region (x-offset 0) (y-offset 0))
  "Intercept replay to use cached DisplayList if available."
  (let ((medium (sheet-medium stream)))
    (if (and (typep medium 'render-stack-medium)
             (medium-buffering-output-p medium))
        (let ((builder (mirror-display-list-builder
                        (climi::sheet-mirror (medium-sheet medium)))))
          (when (or (cached-needs-rebuild record)
                    (null (cached-display-list record)))
            (rebuild-cached-display-list record stream))
          (let ((dl (cached-display-list record)))
            (when dl
              (frs:display-list-builder-save builder)
              (unwind-protect
                   (progn
                     (when region
                       (with-bounding-rectangle* (left top right bottom) region
                         (frs:display-list-builder-clip-rect
                          builder
                          (float left 1.0f0) (float top 1.0f0)
                          (float (abs (- right left)) 1.0f0)
                          (float (abs (- bottom top)) 1.0f0)
                          :intersect)))
                     (when (or (/= x-offset 0) (/= y-offset 0))
                       (frs:display-list-builder-translate
                        builder (float x-offset 1.0f0) (float y-offset 1.0f0)))
                     (frs:display-list-builder-draw-display-list builder dl 1.0f0))
                (frs:display-list-builder-restore builder)))))
        (call-next-method))))

;;; ============================================================================
;;; Integration with invoke-updating-output
;;; ============================================================================

(defmethod invoke-updating-output :around
    ((stream output-recording-stream) continuation record-type
     unique-id id-test cache-value cache-test
     &key &allow-other-keys)
  (let ((port (port stream)))
    (if (and (typep port 'render-stack-port)
             (eq record-type 'standard-updating-output-record))
        (call-next-method stream continuation
                          'render-stack-updating-output-record
                          unique-id id-test cache-value cache-test)
        (call-next-method))))

;;; ============================================================================
;;; Leaf record — prevent infinite recursion in map-over-output-records
;;; ============================================================================

(defmethod map-over-output-records-containing-position
    (function (record climi::basic-output-record) x y
     &optional x-offset y-offset &rest function-args)
  "Leaf record: no children to map over."
  (declare (ignore function record x y x-offset y-offset function-args))
  nil)
