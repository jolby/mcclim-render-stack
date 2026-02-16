;;;; multi-window-delegate.lisp — Multi-Window Render Delegate for McCLIM
;;;;
;;;; This file implements the render-stack delegate protocol for managing
;;;; multiple McCLIM ports/windows with correct threading and event routing.

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; Multi-Window Render Delegate Class
;;; ============================================================================
;;; Task 1.2: Create Multi-Window Render Delegate Class

(defclass multi-window-render-delegate (render-delegate)
  ;; Window Management
  ((window-table :accessor delegate-window-table
                 :initform (make-hash-table :test 'eql)
                 :documentation "Map SDL3 window-id to port")
   (port-table :accessor delegate-port-table
               :initform (make-hash-table :test 'eq)
               :documentation "Map port to window-id")
   
   ;; Thread Safety
   (table-lock :accessor delegate-table-lock
               :initform (bt2:make-lock "delegate-table-lock")
               :documentation "Lock for window/port table modifications")
   
   ;; Display List Management
   (pending-display-lists :accessor delegate-pending-display-lists
                          :initform (make-hash-table :test 'eq)
                          :documentation "Map port → display-list (UI thread writes, main thread reads)")
   (display-list-lock :accessor delegate-display-list-lock
                      :initform (bt2:make-lock "display-list-lock")
                      :documentation "Lock for atomic display list swap")
   
   ;; Frame State
   (dirty-ports :accessor delegate-dirty-ports
                :initform nil
                :documentation "List of ports needing redraw (protected by dirty-lock)")
   (dirty-lock :accessor delegate-dirty-lock
               :initform (bt2:make-lock "dirty-ports-lock")
               :documentation "Lock for dirty-ports list")
   
   ;; Impeller Resources
   (typography-context :accessor delegate-typography-context
                       :initform nil
                       :documentation "Shared typography context"))
  (:documentation "Render delegate that manages multiple McCLIM windows.

Thread Safety:
- All slot access must hold appropriate lock
- Window table reads: Must hold TABLE-LOCK
- Display list operations: Must hold DISPLAY-LIST-LOCK
- Dirty ports operations: Must hold DIRTY-LOCK

The delegate implements the render-stack RENDER-DELEGATE protocol:
- RENDER-DELEGATE-BEGIN-FRAME: UI thread, builds display lists
- RENDER-DELEGATE-END-FRAME: UI thread, cleanup
- RENDER-DELEGATE-NOTIFY-IDLE: UI thread, deferred work
- RENDER-DELEGATE-DRAW: Main thread, rasterization"))

;;; ============================================================================
;;; Task 1.3: Window Registration Protocol
;;; ============================================================================

(defgeneric register-port-with-delegate (delegate port window-id)
  (:documentation "Register a port with the delegate.
   
   Thread Contract: May be called from any thread. Acquires table-lock.
   
   Arguments:
     DELEGATE - The multi-window-render-delegate instance
     PORT - The render-stack-port to register
     WINDOW-ID - The SDL3 window ID (integer)"))

(defmethod register-port-with-delegate ((delegate multi-window-render-delegate)
                                        port
                                        window-id)
  "Register a port with the delegate.
   
   Thread Contract: May be called from any thread. Acquires table-lock."
  (bt2:with-lock-held ((delegate-table-lock delegate))
    (setf (gethash window-id (delegate-window-table delegate)) port
          (gethash port (delegate-port-table delegate)) window-id)))

(defgeneric unregister-port-from-delegate (delegate port)
  (:documentation "Unregister a port from the delegate.
   
   Thread Contract: May be called from any thread. Acquires table-lock and display-list-lock.
   
   Arguments:
     DELEGATE - The multi-window-render-delegate instance
     PORT - The render-stack-port to unregister"))

(defmethod unregister-port-from-delegate ((delegate multi-window-render-delegate)
                                          port)
  "Unregister a port from the delegate.
   
   Thread Contract: May be called from any thread. Acquires table-lock and display-list-lock."
  (bt2:with-lock-held ((delegate-table-lock delegate))
    (let ((window-id (gethash port (delegate-port-table delegate))))
      (when window-id
        (remhash window-id (delegate-window-table delegate))
        (remhash port (delegate-port-table delegate))))
    ;; Also clean up display list
    (bt2:with-lock-held ((delegate-display-list-lock delegate))
      (remhash port (delegate-pending-display-lists delegate)))))

(defgeneric find-port-for-window (delegate window-id)
  (:documentation "Find the port associated with a window ID.
   
   Thread Contract: May be called from any thread. Acquires table-lock.
   
   Arguments:
     DELEGATE - The multi-window-render-delegate instance
     WINDOW-ID - The SDL3 window ID (integer)
   
   Returns: The render-stack-port, or NIL if not found."))

(defmethod find-port-for-window ((delegate multi-window-render-delegate) window-id)
  "Find the port associated with a window ID.
   
   Thread Contract: May be called from any thread. Acquires table-lock."
  (bt2:with-lock-held ((delegate-table-lock delegate))
    (gethash window-id (delegate-window-table delegate))))

;;; ============================================================================
;;; Task 1.4: Render-Delegate Protocol Methods
;;; ============================================================================

(defmethod render-stack:render-delegate-begin-frame 
    ((delegate multi-window-render-delegate) target-time frame-number)
  "Called at frame start on UI thread. Build display lists for dirty ports.
   
   Thread Contract: MUST be called on UI thread."
  (rs-internals:assert-ui-thread render-delegate-begin-frame)
  
  ;; Build display lists for all dirty ports
  (let ((dirty-ports nil))
    ;; Atomically grab and clear dirty list
    (bt2:with-lock-held ((delegate-dirty-lock delegate))
      (setf dirty-ports (delegate-dirty-ports delegate))
      (setf (delegate-dirty-ports delegate) nil))
    
    ;; Build display lists
    (dolist (port dirty-ports)
      (let ((display-list (build-display-list-for-port port target-time)))
        (when display-list
          (bt2:with-lock-held ((delegate-display-list-lock delegate))
            (setf (gethash port (delegate-pending-display-lists delegate))
                  display-list)))))
    
    ;; Return non-nil to indicate we produced content
    (not (null dirty-ports))))

(defun build-display-list-for-port (port target-time)
  "Build a display list for a port. Called on UI thread.
   
   For Phase 1: Just return a test pattern marker.
   For Phase 2: Build from McCLIM output records."
  (declare (ignore port target-time))
  ;; Phase 1: Just mark that this port needs rendering
  :test-pattern)

(defmethod render-stack:render-delegate-end-frame
    ((delegate multi-window-render-delegate) layer-tree frame-timings)
  "Called after frame building completes on UI thread.
   
   Thread Contract: MUST be called on UI thread."
  (declare (ignore layer-tree frame-timings))
  (rs-internals:assert-ui-thread render-delegate-end-frame)
  ;; Phase 1: Nothing to do
  ;; Phase 2: Cleanup, log timing data
  nil)

(defmethod render-stack:render-delegate-notify-idle
    ((delegate multi-window-render-delegate) deadline)
  "Called when engine is idle on UI thread.
   
   Thread Contract: MUST be called on UI thread."
  (declare (ignore deadline))
  (rs-internals:assert-ui-thread render-delegate-notify-idle)
  ;; Can do deferred work here, but must complete before deadline
  nil)

(defmethod render-stack:render-delegate-draw
    ((delegate multi-window-render-delegate) pipeline-item)
  "Called on main/raster thread to render. This is where ALL Impeller work happens.
   
   NOTE: This is a SECONDARY drain point (belt-and-suspenders). The PRIMARY event
   drain is in the port's main-thread-loop which calls drain-sdl3-events directly.
   
   Thread Contract: MUST be called on main thread (SDL3/GL requirement)."
  (rs-internals:assert-main-thread render-delegate-draw)
  
  ;; Secondary drain - events already drained in main-thread-loop, but this ensures
  ;; events are fresh when a frame IS being drawn
  (drain-sdl3-events delegate)
  
  ;; Render each visible window
  (bt2:with-lock-held ((delegate-table-lock delegate))
    (maphash (lambda (window-id port)
               (declare (ignore window-id))
               (render-port-display-list delegate port))
             (delegate-window-table delegate)))
  
  ;; Swap buffers for all windows
  (swap-all-window-buffers delegate))

(defun get-window-id-from-sdl3-event (event-ptr event-type)
  "Extract window ID from SDL3 event based on event type.
   
   Different event types store window ID in different struct fields,
   so we need to dispatch to the appropriate accessor."
  (case event-type
    ((:key-down :key-up)
     (rs-sdl3:keyboard-event-window-id event-ptr))
    ((:mouse-button-down :mouse-button-up)
     (rs-sdl3:mouse-button-event-window-id event-ptr))
    ((:mouse-motion)
     (rs-sdl3:mouse-motion-event-window-id event-ptr))
    ((:mouse-wheel)
     (rs-sdl3:mouse-wheel-event-window-id event-ptr))
    ;; Window events all use the same accessor
    ((:window-close-requested :window-resized :window-exposed 
      :window-focus-gained :window-focus-lost :window-shown 
      :window-hidden :window-moved :window-mouse-enter 
      :window-mouse-leave :window-pixel-size-changed)
     (rs-sdl3:window-event-window-id event-ptr))
    ;; Default: return nil for events without window ID
    (t nil)))

(defun drain-sdl3-events (delegate)
  "Poll SDL3 events and route to appropriate port.
   Calls distribute-event which puts events into McCLIM's per-sheet concurrent-queue.
   
   Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread drain-sdl3-events)
  
  (rs-sdl3:with-sdl3-event (ev)
    (loop while (rs-sdl3:poll-event ev)
          do (let* ((event-type (rs-sdl3:get-event-type ev))
                    (window-id (get-window-id-from-sdl3-event ev event-type))
                    (port (when window-id (find-port-for-window delegate window-id)))
                    (clim-event (when port (translate-sdl3-event port ev))))
               (when (and port clim-event)
                 ;; This routes to McCLIM's concurrent-queue via condition-notify wakeup
                 (distribute-event port clim-event))))))

(defun render-port-display-list (delegate port)
  "Render a port's display list.
   
   Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread render-port-display-list)
  
  ;; Atomically get and clear display list
  (let ((display-list nil))
    (bt2:with-lock-held ((delegate-display-list-lock delegate))
      (setf display-list (gethash port (delegate-pending-display-lists delegate)))
      (when display-list
        (remhash port (delegate-pending-display-lists delegate))))
    
    (when display-list
      ;; Phase 1: Just draw test pattern
      ;; Phase 2: Execute actual display list
      (draw-test-pattern-for-port port))))

(defun draw-test-pattern-for-port (port)
  "Draw a simple test pattern for a port.
   
   Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread draw-test-pattern-for-port)
  
  ;; Get window dimensions
  (let* ((window (port-window port))
         (width (if window 
                    (rs-host:framebuffer-width window)
                    800))  ; Default fallback
         (height (if window
                     (rs-host:framebuffer-height window)
                     600)))  ; Default fallback
    
    ;; Clear to white
    (frs:with-display-list-builder (builder)
      (let ((paint (frs:make-paint)))
        (frs:paint-set-color paint 1.0 1.0 1.0 1.0)
        (frs:draw-rect builder 0.0 0.0 
                       (float width 1.0f0) (float height 1.0f0)
                       paint)
        (frs:release-paint paint))
      
      ;; Draw a colored rectangle (test pattern)
      (let ((paint (frs:make-paint)))
        (frs:paint-set-color paint 0.2 0.4 0.9 1.0)
        (frs:draw-rect builder 50.0 50.0 200.0 150.0 paint)
        (frs:release-paint paint))
      
      ;; Execute
      (let ((surface (get-port-surface port)))
        (when surface
          (frs:execute-display-list surface builder))))))

(defun swap-all-window-buffers (delegate)
  "Swap GL buffers for all windows.
   
   Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread swap-all-window-buffers)
  
  (bt2:with-lock-held ((delegate-table-lock delegate))
    (maphash (lambda (window-id port)
               (declare (ignore window-id))
               (let ((window (port-window port)))
                 (when window
                   (%sdl3:gl-swap-window 
                    (rs-sdl3::sdl3-window-handle window)))))
             (delegate-window-table delegate))))

;;; Helper to get/create Impeller surface for a port
(defun get-port-surface (port)
  "Get or create the Impeller surface for a port.
   
   Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread get-port-surface)
  
  ;; For now, we need to get the surface from the port's delegate
  ;; This is a placeholder - in the full implementation, the port would
  ;; manage its own surface
  (let ((window (port-window port)))
    (when window
      ;; Create surface on demand - in Phase 2, this would be cached
      (let ((width (rs-host:framebuffer-width window))
            (height (rs-host:framebuffer-height window)))
        ;; Return a wrapped FBO surface
        ;; Note: In full implementation, we'd cache this per-port
        (frs:make-wrapped-fbo-surface 
         (or (get-typography-context) (frs:make-typography-context))
         0  ; FBO 0 = default framebuffer
         width height)))))

(defun get-typography-context ()
  "Get the global typography context, creating if needed.
   
   Thread Contract: Any thread (uses global)."
  (when *global-delegate*
    (delegate-typography-context *global-delegate*)))

;;; ============================================================================
;;; Event Translation (moved from port.lisp)
;;; ============================================================================

(defun translate-sdl3-event (port event)
  "Translate a single SDL3 event to a CLIM event.
   Returns a CLIM event object, or NIL if the event should be ignored.
   
   Thread Contract: Called from drain-sdl3-events which runs on main thread."
  (let* ((event-type (rs-sdl3:get-event-type event))
         (window (port-window port))
         (sheet (when window (port-find-sheet-for-window port window))))
    ;; Drop events if no CLIM sheet is registered yet
    (unless sheet
      (return-from translate-sdl3-event nil))
    (case event-type
      (:quit
       (setf (port-quit-requested port) t)
       (make-instance 'window-destroy-event
                      :sheet sheet))

      (:window-close-requested
       (setf (port-quit-requested port) t)
       (make-instance 'window-manager-delete-event
                      :sheet sheet))

       (:window-resized
        ;; Window was resized - update surface and create configuration event
        ;; Get new size from window
        (let ((width (rs-host:window-width window))
              (height (rs-host:window-height window)))
          (make-instance 'window-configuration-event
                         :sheet sheet
                         :region (clim:make-bounding-rectangle 0 0 width height))))

      (:window-exposed
       ;; Window needs repaint
       (make-instance 'window-repaint-event
                      :sheet sheet
                      :region clim:+everywhere+))

      (:window-focus-gained
       (make-instance 'window-manager-focus-event
                      :sheet sheet))

      (:window-focus-lost
       nil)

      (:window-shown
       (make-instance 'window-map-event
                      :sheet sheet))

      (:window-hidden
       (make-instance 'window-unmap-event
                      :sheet sheet))

      (:window-mouse-enter
       (let ((pointer (port-pointer port)))
         (make-instance 'pointer-enter-event
                        :sheet sheet
                        :pointer pointer)))

      (:window-mouse-leave
       (let ((pointer (port-pointer port)))
         (make-instance 'pointer-exit-event
                        :sheet sheet
                        :pointer pointer)))

      (:key-down
       (let ((keycode (rs-sdl3:keyboard-event-keycode event))
             (modifiers (rs-sdl3:keyboard-event-mod event)))
         ;; Handle escape as quit
         (when (= keycode %sdl3:+k-escape+)
           (setf (port-quit-requested port) t))
         ;; Create key press event
         (let ((key-name (sdl3-keycode-to-key-name keycode))
               (mod-state (sdl3-modifiers-to-clim modifiers)))
           (when key-name
             (make-instance 'key-press-event
                            :sheet sheet
                            :key-name key-name
                            :key-character (sdl3-keycode-to-character keycode mod-state)
                            :x 0
                            :y 0
                            :modifier-state mod-state)))))

      (:key-up
       (let ((keycode (rs-sdl3:keyboard-event-keycode event))
             (modifiers (rs-sdl3:keyboard-event-mod event)))
         (let ((key-name (sdl3-keycode-to-key-name keycode))
               (mod-state (sdl3-modifiers-to-clim modifiers)))
           (when key-name
             (make-instance 'key-release-event
                            :sheet sheet
                            :key-name key-name
                            :key-character (sdl3-keycode-to-character keycode mod-state)
                            :x 0
                            :y 0
                            :modifier-state mod-state)))))

      (:mouse-button-down
       (let* ((button (rs-sdl3:mouse-button-event-button event))
              (x (rs-sdl3:mouse-button-event-x event))
              (y (rs-sdl3:mouse-button-event-y event))
              (clim-button (sdl3-button-to-clim-button button))
              (pointer (port-pointer port)))
         (when clim-button
           (update-pointer-button-state pointer clim-button t)
           (update-pointer-position pointer x y)
           (make-instance 'pointer-button-press-event
                          :sheet sheet
                          :pointer pointer
                          :button (sdl3-button-to-clim-constant button)
                          :x x
                          :y y
                          :modifier-state 0))))

      (:mouse-button-up
       (let* ((button (rs-sdl3:mouse-button-event-button event))
              (x (rs-sdl3:mouse-button-event-x event))
              (y (rs-sdl3:mouse-button-event-y event))
              (clim-button (sdl3-button-to-clim-button button))
              (pointer (port-pointer port)))
         (when clim-button
           (update-pointer-button-state pointer clim-button nil)
           (update-pointer-position pointer x y)
           (make-instance 'pointer-button-release-event
                          :sheet sheet
                          :pointer pointer
                          :button (sdl3-button-to-clim-constant button)
                          :x x
                          :y y
                          :modifier-state 0))))

      (:mouse-motion
       (let* ((x (rs-sdl3:mouse-motion-event-x event))
              (y (rs-sdl3:mouse-motion-event-y event))
              (pointer (port-pointer port)))
         (update-pointer-position pointer x y)
         (make-instance 'pointer-motion-event
                        :sheet sheet
                        :pointer pointer
                        :x x
                        :y y
                        :modifier-state 0)))

      (:mouse-wheel
       (let* ((x (rs-sdl3:mouse-wheel-event-x event))
              (y (rs-sdl3:mouse-wheel-event-y event))
              (delta-x x)
              (delta-y y)
              (pointer (port-pointer port)))
         (make-instance 'pointer-scroll-event
                        :sheet sheet
                        :pointer pointer
                        :delta-x delta-x
                        :delta-y delta-y)))

      ;; Ignore unhandled events
      (t nil))))

;;; Event Translation Helpers (from port.lisp)

(defun sdl3-modifiers-to-clim (sdl3-mod)
  "Convert SDL3 modifier bitmask to CLIM modifier state.
   SDL3 uses bitmask: shift=3, ctrl=192, alt=768, gui=1024/2048"
  (let ((mod 0))
    (when (plusp (logand sdl3-mod 3))   ; Either shift
      (setf mod (logior mod clim:+shift-key+)))
    (when (plusp (logand sdl3-mod 192)) ; Either ctrl
      (setf mod (logior mod clim:+control-key+)))
    (when (plusp (logand sdl3-mod 768)) ; Either alt
      (setf mod (logior mod clim:+meta-key+)))
    (when (plusp (logand sdl3-mod 1024)) ; Left gui/super
      (setf mod (logior mod clim:+super-key+)))
    (when (plusp (logand sdl3-mod 2048)) ; Right gui/super
      (setf mod (logior mod clim:+hyper-key+)))
    mod))

(defun sdl3-keycode-to-key-name (keycode)
  "Convert SDL3 keycode to CLIM key-name symbol.
   Based on SDL3 scancode constants."
  (case keycode
    ;; Function keys
    (#.%sdl3:+k-f1+ :f1)
    (#.%sdl3:+k-f2+ :f2)
    (#.%sdl3:+k-f3+ :f3)
    (#.%sdl3:+k-f4+ :f4)
    (#.%sdl3:+k-f5+ :f5)
    (#.%sdl3:+k-f6+ :f6)
    (#.%sdl3:+k-f7+ :f7)
    (#.%sdl3:+k-f8+ :f8)
    (#.%sdl3:+k-f9+ :f9)
    (#.%sdl3:+k-f10+ :f10)
    (#.%sdl3:+k-f11+ :f11)
    (#.%sdl3:+k-f12+ :f12)

    ;; Arrow keys
    (#.%sdl3:+k-up+ :up)
    (#.%sdl3:+k-down+ :down)
    (#.%sdl3:+k-right+ :right)
    (#.%sdl3:+k-left+ :left)

    ;; Special keys
    (#.%sdl3:+k-return+ :return)
    (#.%sdl3:+k-escape+ :escape)
    (#.%sdl3:+k-backspace+ :backspace)
    (#.%sdl3:+k-tab+ :tab)
    (#.%sdl3:+k-space+ :space)
    (#.%sdl3:+k-capslock+ :caps-lock)
    (#.%sdl3:+k-scrolllock+ :scroll-lock)
    (#.%sdl3:+k-numlockclear+ :numlock)
    (#.%sdl3:+k-printscreen+ :print)
    (#.%sdl3:+k-pause+ :pause)
    (#.%sdl3:+k-delete+ :delete)
    (#.%sdl3:+k-insert+ :insert)
    (#.%sdl3:+k-home+ :home)
    (#.%sdl3:+k-end+ :end)
    (#.%sdl3:+k-pageup+ :page-up)
    (#.%sdl3:+k-pagedown+ :page-down)

    ;; Character keys - mapped to their base character
    (otherwise
     ;; For letters and numbers, return the character
     (cond
       ((>= keycode 97) (- keycode 32))  ; a-z → A-Z
       ((>= keycode 65) keycode)          ; A-Z
       ((>= keycode 48) keycode)          ; 0-9
       (t nil)))))

(defun sdl3-keycode-to-character (keycode modifiers)
  "Convert SDL3 keycode + modifiers to character, or NIL if not printable.
   This is a simplified version - full IME support would be more complex."
  (let ((char (sdl3-keycode-to-key-name keycode)))
    (when (and char
                (typep char 'character)
                (not (plusp modifiers)))
      char)))
