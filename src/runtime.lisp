;;;; runtime.lisp — render-stack-runtime class and initialization
;;;;
;;;; Consolidates global engine, delegate, and Impeller context into a single
;;;; CLOS object stored on the port. The runtime IS the render-delegate.
;;;;
;;;; Thread Safety Model:
;;;; - engine: Read-only after initialization, safe from any thread
;;;; - impeller-context: Read-only after initialization, safe from any thread
;;;; - typography-context: Read-only after initialization, safe from any thread
;;;; - window-registry: Protected by registry-lock
;;;; - dirty-sheets: Protected by dirty-lock

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; GL Proc Address Callback for Impeller
;;; ============================================================================
;;;
;;; Impeller's C code needs to resolve OpenGL ES entry points at context
;;; creation time. We register this CFFI callback so Impeller can query
;;; the GL function pointers via SDL3.

(cffi:defcallback sdl3-gl-proc-getter :pointer
    ((proc-name :string) (user-data :pointer))
  (declare (ignore user-data))
  (%sdl3:gl-get-proc-address proc-name))

;;; ============================================================================
;;; render-stack-runtime Class
;;; ============================================================================
;;;
;;; This class consolidates the render-stack engine, Impeller context,
;;; and window registry into a single object that is both:
;;; 1. A render-delegate (implements the render protocol)
;;; 2. A stateful container for all display state
;;;
;;; It replaces the previous architecture's global variables:
;;;   *global-engine* → (runtime-engine runtime)
;;;   *global-delegate* → runtime (itself)
;;;   *global-impeller-context* → (runtime-impeller-context runtime)
;;;   *global-engine-initialized* → (runtime-initialized-p runtime)

(defclass render-stack-runtime (render-delegate)
  ((engine
    :accessor runtime-engine
    :initform nil
    :documentation "The render-engine instance. Created during initialize-runtime.")
   
   (impeller-context
    :accessor runtime-impeller-context
    :initform nil
    :documentation "Impeller GL rendering context. Created in realize-mirror after first SDL3 window.")
   
   (typography-context
    :accessor runtime-typography-context
    :initform nil
    :documentation "Impeller typography context for text rendering.")
   
   (window-registry
    :accessor runtime-window-registry
    :initform (make-hash-table :test 'eql)
    :documentation "Map SDL3 window-id (integer) → CLIM sheet for event routing.")
   
   (registry-lock
    :accessor runtime-registry-lock
    :initform (bt2:make-lock :name "runtime-registry")
    :documentation "Lock protecting window-registry hash table.")
   
   (dirty-sheets
    :accessor runtime-dirty-sheets
    :initform nil
    :documentation "List of sheets needing redraw (protected by dirty-lock).")
   
   (dirty-lock
    :accessor runtime-dirty-lock
    :initform (bt2:make-lock :name "runtime-dirty")
    :documentation "Lock protecting dirty-sheets list.")
   
   (initialized-p
    :accessor runtime-initialized-p
    :initform nil
    :documentation "T if engine and typography context have been initialized."))
  (:documentation "Consolidated render-stack runtime state for McCLIM.

This is the central stateful object that replaces the previous global-variable
architecture. It is stored on the port via (port-runtime port) and accessed by
runner phases, mirror creation, and event routing.

Implements the render-delegate protocol directly — no separate delegate object."))

;;; ============================================================================
;;; Initialization
;;; ============================================================================

(defgeneric initialize-runtime (runtime port)
  (:documentation "Initialize the render-engine and typography-context on the main thread.

RUNTIME: The render-stack-runtime instance
PORT: The render-stack-port (used to store host and other initialization state)

Thread Contract: MUST be called on the main thread.
Called from port initialize-instance :after (via rs-internals:submit-to-main-thread)"))

(defmethod initialize-runtime ((runtime render-stack-runtime) port)
  "Initialize render-engine and typography-context.

MUST be called on the main thread. Idempotent — safe to call multiple times."
  (rs-internals:assert-main-thread initialize-runtime)
  (unless (runtime-initialized-p runtime)
    (log:info :mcclim-render-stack "Initializing runtime on main thread")
    
    ;; Initialize SDL3 video system
    (rs-sdl3:init-sdl3-video)
    
    ;; Create and register the SDL3 host
    (setf (port-host port) (make-instance 'rs-sdl3:sdl3-host))
    
    ;; Create typography context (can be created before GL context)
    (setf (runtime-typography-context runtime)
          (frs:make-typography-context))
    
    ;; Create and start the render engine
    ;; Pass 'runtime' as the delegate — runtime IS the delegate
    (setf (runtime-engine runtime)
          (render-stack:make-render-engine :delegate runtime
                                           :pipeline-depth 2
                                           :target-fps 60))
    (render-stack:render-engine-start (runtime-engine runtime))
    
    ;; Mark as initialized
    (setf (runtime-initialized-p runtime) t)
    (log:info :mcclim-render-stack "Runtime initialization complete")))

(defgeneric initialize-runtime-impeller-context (runtime)
  (:documentation "Create the Impeller GL rendering context on the main thread.

RUNTIME: The render-stack-runtime instance

Thread Contract: MUST be called on the main thread.
Called from realize-mirror after SDL3 GL context is current.
Idempotent — created only once."))

(defmethod initialize-runtime-impeller-context ((runtime render-stack-runtime))
  "Create Impeller GL context (after SDL3 GL context is current).

MUST be called on the main thread. Idempotent."
  (rs-internals:assert-main-thread initialize-runtime-impeller-context)
  (unless (runtime-impeller-context runtime)
    (log:info :mcclim-render-stack "Creating Impeller GL context")
    (setf (runtime-impeller-context runtime)
          (rs-internals:without-float-traps
            (frs:make-context :gl-proc-address-callback
                              (cffi:callback sdl3-gl-proc-getter))))
    (log:info :mcclim-render-stack "Impeller GL context created: ~A"
              (runtime-impeller-context runtime))))

;;; ============================================================================
;;; Window Registry Protocol
;;; ============================================================================
;;;
;;; Thread-safe window ID ↔ sheet mapping for event routing.

(defgeneric register-window (runtime window-id sheet)
  (:documentation "Register an SDL3 window ID with its corresponding CLIM sheet.

Thread Contract: May be called from any thread. Acquires registry-lock."))

(defmethod register-window ((runtime render-stack-runtime) window-id sheet)
  "Register a window in the runtime registry."
  (bt2:with-lock-held ((runtime-registry-lock runtime))
    (setf (gethash window-id (runtime-window-registry runtime)) sheet)))

(defgeneric unregister-window (runtime window-id)
  (:documentation "Unregister an SDL3 window ID.

Thread Contract: May be called from any thread. Acquires registry-lock."))

(defmethod unregister-window ((runtime render-stack-runtime) window-id)
  "Unregister a window from the runtime registry."
  (bt2:with-lock-held ((runtime-registry-lock runtime))
    (remhash window-id (runtime-window-registry runtime))))

(defgeneric find-sheet-for-window (runtime window-id)
  (:documentation "Look up the CLIM sheet for an SDL3 window ID.

Returns the sheet, or NIL if not found.
Thread Contract: May be called from any thread. Acquires registry-lock."))

(defmethod find-sheet-for-window ((runtime render-stack-runtime) window-id)
  "Look up a sheet by window ID."
  (bt2:with-lock-held ((runtime-registry-lock runtime))
    (gethash window-id (runtime-window-registry runtime))))

;;; ============================================================================
;;; Dirty Sheet Tracking
;;; ============================================================================
;;;
;;; Track which sheets need redraw in the next frame.

(defgeneric mark-sheet-dirty (runtime sheet)
  (:documentation "Mark a sheet as needing redraw.

Thread Contract: May be called from any thread. Acquires dirty-lock."))

(defmethod mark-sheet-dirty ((runtime render-stack-runtime) sheet)
  "Add a sheet to the dirty list if not already present."
  (bt2:with-lock-held ((runtime-dirty-lock runtime))
    (unless (member sheet (runtime-dirty-sheets runtime))
      (push sheet (runtime-dirty-sheets runtime)))))

(defgeneric grab-and-clear-dirty-sheets (runtime)
  (:documentation "Atomically grab the dirty sheet list and clear it.

Returns the list of dirty sheets.
Thread Contract: Should be called from UI thread (called in begin-frame)."))

(defmethod grab-and-clear-dirty-sheets ((runtime render-stack-runtime))
  "Get dirty sheets and clear the list (atomic operation)."
  (bt2:with-lock-held ((runtime-dirty-lock runtime))
    (let ((sheets (runtime-dirty-sheets runtime)))
      (setf (runtime-dirty-sheets runtime) nil)
      sheets)))

;;; ============================================================================
;;; Render-Delegate Protocol Implementation
;;; ============================================================================
;;;
;;; The runtime IS the render-delegate. It implements all four protocol methods.

(defmethod render-stack:render-delegate-begin-frame
    ((runtime render-stack-runtime) target-time frame-number)
  "Called on UI thread at frame start. Build display lists for dirty sheets.

Thread Contract: MUST be called on UI thread."
  (rs-internals:assert-ui-thread render-delegate-begin-frame)
  (let ((dirty-sheets (grab-and-clear-dirty-sheets runtime)))
    (format *error-output* "~&[DIAG] render-delegate-begin-frame: ~
                             frame ~A, dirty-sheets: ~A~%"
            frame-number (length dirty-sheets))
    ;; Return non-nil if we produced content (Phase 1: always have dirty sheets)
    (not (null dirty-sheets))))

(defmethod render-stack:render-delegate-end-frame
    ((runtime render-stack-runtime) layer-tree frame-timings)
  "Called on UI thread after frame building.

Thread Contract: MUST be called on UI thread."
  (declare (ignore layer-tree frame-timings))
  (rs-internals:assert-ui-thread render-delegate-end-frame)
  ;; Phase 1: No cleanup needed
  nil)

(defmethod render-stack:render-delegate-notify-idle
    ((runtime render-stack-runtime) deadline)
  "Called on UI thread when engine is idle.

Thread Contract: MUST be called on UI thread."
  (declare (ignore deadline))
  (rs-internals:assert-ui-thread render-delegate-notify-idle)
  ;; Phase 1: No deferred work
  nil)

(defmethod render-stack:render-delegate-draw
    ((runtime render-stack-runtime) pipeline-item)
  "Called on main thread to render.

Thread Contract: MUST be called on main thread."
  (declare (ignore pipeline-item))
  (rs-internals:assert-main-thread render-delegate-draw)
  (format *error-output* "~&[DIAG] render-delegate-draw: window-count=~A~%"
          (hash-table-count (runtime-window-registry runtime)))
  
  ;; Render each registered window with a test pattern (white bg + blue rect)
  (bt2:with-lock-held ((runtime-registry-lock runtime))
    (maphash (lambda (window-id sheet)
               (declare (ignore window-id))
               ;; Use standard CLIM API to traverse sheet → port
               (let ((port (clim:port sheet)))
                 (when port
                   (draw-test-pattern-for-port-runtime port runtime))))
             (runtime-window-registry runtime)))
  
  ;; Swap buffers
  (swap-all-window-buffers-in-runtime runtime))

;;; ============================================================================
;;; Direct Rendering (Phase 1)
;;; ============================================================================

(defun draw-test-pattern-for-port-runtime (port runtime)
  "Draw a test pattern (white bg + blue rect) for a port using the runtime's Impeller context.

Thread Contract: MUST be called on main thread."
  (declare (ignore runtime))
  (rs-internals:assert-main-thread draw-test-pattern-for-port-runtime)
  (let ((window (port-window port)))
    (when window
      (let ((width (rs-host:framebuffer-width window))
            (height (rs-host:framebuffer-height window)))
        ;; Build and execute display list
        (frs:with-display-list-builder (builder)
          ;; White background
          (let ((paint (frs:make-paint)))
            (frs:paint-set-color paint 1.0 1.0 1.0 1.0)
            (frs:draw-rect builder 0.0 0.0
                           (float width 1.0f0) (float height 1.0f0)
                           paint)
            (frs:release-paint paint))
          
          ;; Blue rectangle (test pattern)
          (let ((paint (frs:make-paint)))
            (frs:paint-set-color paint 0.2 0.4 0.9 1.0)
            (frs:draw-rect builder 50.0 50.0 200.0 150.0 paint)
            (frs:release-paint paint))
          
          ;; Get or create surface and execute
          (let ((surface (get-port-surface port)))
            (when surface
              (rs-internals:without-float-traps
                (frs:execute-display-list surface builder)))))))))

(defun swap-all-window-buffers-in-runtime (runtime)
  "Swap GL buffers for all windows registered in the runtime.

Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread swap-all-window-buffers-in-runtime)
  (bt2:with-lock-held ((runtime-registry-lock runtime))
    (maphash (lambda (window-id sheet)
               (declare (ignore window-id))
               (let ((port (clim:port sheet)))
                 (when port
                   (let ((window (port-window port)))
                     (when window
                       (rs-sdl3:sdl3-gl-swap-window window))))))
             (runtime-window-registry runtime))))

;;; ============================================================================
;;; Cleanup
;;; ============================================================================

(defun shutdown-runtime (runtime)
  "Shutdown the runtime engine and release resources.

Should be called on application exit, on the main thread."
  (when (runtime-engine runtime)
    (log:info :mcclim-render-stack "Shutting down render engine")
    (render-stack:render-engine-stop (runtime-engine runtime))
    (setf (runtime-engine runtime) nil))
  
  (when (runtime-impeller-context runtime)
    (log:info :mcclim-render-stack "Releasing Impeller context")
    (rs-internals:without-float-traps
      (frs:release-context (runtime-impeller-context runtime)))
    (setf (runtime-impeller-context runtime) nil))
  
  (when (runtime-typography-context runtime)
    (log:info :mcclim-render-stack "Releasing typography context")
    (frs:release-typography-context (runtime-typography-context runtime))
    (setf (runtime-typography-context runtime) nil))
  
  (setf (runtime-initialized-p runtime) nil)
  (log:info :mcclim-render-stack "Runtime shutdown complete"))
