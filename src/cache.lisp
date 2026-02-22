;;;; cache.lisp — LRU cache for Impeller paragraphs and other GPU resources
;;;;
;;;; Paragraphs are expensive to create (Impeller typography pipeline).
;;;; This generic LRU cache is keyed by text+style+ink and lives on the port,
;;;; shared across all mediums for that port.
;;;;
;;;; Thread safety: all public functions hold a per-cache lock.
;;;; Eviction: on capacity overflow the LRU entry is evicted synchronously
;;;; inside cache-put, invoking an optional on-evict callback for resource cleanup.

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; LRU Cache — doubly-linked list + hash table
;;; ============================================================================

(defclass lru-cache-node ()
  ((key   :initarg :key   :accessor node-key)
   (value :initarg :value :accessor node-value)
   (prev  :initform nil   :accessor node-prev)
   (next  :initform nil   :accessor node-next)))

(defclass lru-cache ()
  ((capacity :initarg :capacity :initform 1000 :accessor cache-capacity)
   (table    :initform (make-hash-table :test 'equal) :reader cache-table)
   (head     :initform nil :accessor cache-head)
   (tail     :initform nil :accessor cache-tail)
   (lock     :initform (bt2:make-lock :name "lru-cache-lock") :reader cache-lock)))

(defun %lru-remove-node (cache node)
  "Splice NODE out of CACHE's doubly-linked list."
  (let ((prev (node-prev node))
        (next (node-next node)))
    (if prev
        (setf (node-next prev) next)
        (setf (cache-head cache) next))
    (if next
        (setf (node-prev next) prev)
        (setf (cache-tail cache) prev))))

(defun %lru-add-to-head (cache node)
  "Insert NODE at the head (most-recently-used end) of CACHE's list."
  (setf (node-next node) (cache-head cache)
        (node-prev node) nil)
  (if (cache-head cache)
      (setf (node-prev (cache-head cache)) node)
      (setf (cache-tail cache) node))
  (setf (cache-head cache) node))

(defun cache-get (cache key)
  "Return the value for KEY in CACHE, promoting it to most-recently-used.
   Returns NIL on a cache miss."
  (bt2:with-lock-held ((cache-lock cache))
    (let ((node (gethash key (cache-table cache))))
      (when node
        (%lru-remove-node cache node)
        (%lru-add-to-head cache node)
        (node-value node)))))

(defun cache-put (cache key value &optional on-evict)
  "Store VALUE under KEY in CACHE.
   If KEY already exists its value is updated in place.
   If the cache is at capacity the least-recently-used entry is evicted;
   ON-EVICT (if supplied) is called with the evicted value."
  (bt2:with-lock-held ((cache-lock cache))
    (let ((existing (gethash key (cache-table cache))))
      (if existing
          (progn
            (%lru-remove-node cache existing)
            (%lru-add-to-head cache existing)
            (setf (node-value existing) value))
          (let ((node (make-instance 'lru-cache-node :key key :value value)))
            (setf (gethash key (cache-table cache)) node)
            (%lru-add-to-head cache node)
            (when (> (hash-table-count (cache-table cache)) (cache-capacity cache))
              (let ((lru (cache-tail cache)))
                (when lru
                  (remhash (node-key lru) (cache-table cache))
                  (%lru-remove-node cache lru)
                  (when on-evict
                    (funcall on-evict (node-value lru)))))))))))

(defun cache-clear (cache &optional on-evict)
  "Remove all entries from CACHE.
   ON-EVICT (if supplied) is called once per evicted value."
  (bt2:with-lock-held ((cache-lock cache))
    (when on-evict
      (maphash (lambda (key node)
                 (declare (ignore key))
                 (funcall on-evict (node-value node)))
               (cache-table cache)))
    (clrhash (cache-table cache))
    (setf (cache-head cache) nil
          (cache-tail cache) nil)))

;;; ============================================================================
;;; Paragraph Cache — typed wrapper around lru-cache
;;; ============================================================================

(defstruct cached-paragraph
  "An Impeller paragraph retained in the cache, together with its layout metrics."
  paragraph
  height
  baseline
  width)

(defun make-paragraph-cache (&key (capacity 1000))
  "Create a new LRU cache for Impeller paragraph objects."
  (make-instance 'lru-cache :capacity capacity))

(defun clear-paragraph-cache (cache)
  "Evict all entries from CACHE, releasing the underlying Impeller paragraphs."
  (cache-clear cache
               (lambda (entry)
                 (frs:release-paragraph (cached-paragraph-paragraph entry)))))
