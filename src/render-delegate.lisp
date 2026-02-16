;;;; render-delegate.lisp — McCLIM Port as render-stack Delegate
;;;;
;;;; This is the key integration piece: the McCLIM port implements
;;;; render-stack's render-delegate protocol, allowing the render engine
;;;; to drive McCLIM's frame loop.

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; SDL3 Callback for OpenGL Proc Address Resolution
;;; ============================================================================

(cffi:defcallback gl-proc-address-getter :pointer
    ((proc-name :string) (user-data :pointer))
  "Resolve GL function address via SDL3."
  (declare (ignore user-data))
  (%sdl3:gl-get-proc-address proc-name))

(defclass clim-render-delegate (render-delegate)
  ((port :initarg :port :reader delegate-port)
   (window :initarg :window :reader delegate-window)
   (impeller-context :accessor delegate-impeller-context)
   (impeller-surface :accessor delegate-impeller-surface :initform nil)
   (current-builder :accessor delegate-current-builder :initform nil
                    :documentation "Current display list builder for medium drawing.")
   (frame-count :accessor delegate-frame-count :initform 0))
  (:documentation "Render delegate that draws McCLIM content via Impeller."))

(defmethod initialize-instance :after ((delegate clim-render-delegate) &key)
  "Initialize Impeller context after delegate creation."
  ;; Create Impeller context with GL proc getter
  (setf (delegate-impeller-context delegate)
        (frs:make-context :gl-proc-address-callback 
                          (cffi:callback gl-proc-address-getter)))
  ;; Create surface for current window size
  (update-delegate-surface delegate))

(defun update-delegate-surface (delegate)
  "Create or recreate the Impeller surface for the delegate's window."
  (when (delegate-impeller-surface delegate)
    (frs:release-surface (delegate-impeller-surface delegate)))
  (let* ((window (delegate-window delegate))
         (width (rs-host:framebuffer-width window))
         (height (rs-host:framebuffer-height window)))
    (setf (delegate-impeller-surface delegate)
          (frs:make-wrapped-fbo-surface (delegate-impeller-context delegate)
                                        0 ; FBO 0 = default framebuffer
                                        width height))))

;;; ============================================================================
;;; Event Translation (moved from port.lisp)
;;; ============================================================================

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

(defun translate-sdl3-event (port event)
  "Translate a single SDL3 event to a CLIM event.
   Returns a CLIM event object, or NIL if the event should be ignored."
  (let ((event-type (rs-sdl3:get-event-type event)))
    (case event-type
      (:quit
       (setf (port-quit-requested port) t)
       (make-instance 'window-destroy-event
                      :sheet (port-window port)))

      (:window-close-requested
       (setf (port-quit-requested port) t)
       (make-instance 'window-manager-delete-event
                      :sheet (port-window port)))

      (:window-resized
       (when (port-delegate port)
         (update-delegate-surface (port-delegate port)))
       (let* ((window (port-window port))
              (width (rs-host:window-width window))
              (height (rs-host:window-height window)))
         (make-instance 'window-configuration-event
                        :sheet window
                        :x 0 :y 0
                        :width width
                        :height height)))

      (:window-exposed
       (make-instance 'window-repaint-event
                      :sheet (port-window port)
                      :region clim:+everywhere+))

      (:window-focus-gained
       (make-instance 'window-manager-focus-event
                      :sheet (port-window port)))

      (:window-focus-lost
       nil)

      (:window-shown
       (make-instance 'window-map-event
                      :sheet (port-window port)))

      (:window-hidden
       (make-instance 'window-unmap-event
                      :sheet (port-window port)))

      (:window-mouse-enter
       (let ((pointer (port-pointer port)))
         (make-instance 'pointer-enter-event
                        :sheet (port-window port)
                        :pointer pointer)))

      (:window-mouse-leave
       (let ((pointer (port-pointer port)))
         (make-instance 'pointer-exit-event
                        :sheet (port-window port)
                        :pointer pointer)))

      (:key-down
       (let ((keycode (rs-sdl3:keyboard-event-keycode event))
             (modifiers (rs-sdl3:keyboard-event-mod event)))
         (when (= keycode %sdl3:+k-escape+)
           (setf (port-quit-requested port) t))
         (let ((key-name (sdl3-keycode-to-key-name keycode))
               (mod-state (sdl3-modifiers-to-clim modifiers)))
           (when key-name
             (make-instance 'key-press-event
                           :sheet (port-window port)
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
                           :sheet (port-window port)
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
                          :sheet (port-window port)
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
                          :sheet (port-window port)
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
                        :sheet (port-window port)
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
                        :sheet (port-window port)
                        :pointer pointer
                        :delta-x delta-x
                        :delta-y delta-y)))

      (t nil))))

(defun drain-sdl3-events (port)
  "Poll all pending SDL3 events on the main thread, translate to CLIM events,
   and distribute them to the appropriate sheet queues via McCLIM's
   distribute-event protocol.

   MUST be called from the main thread (SDL3 event polling requirement).
   Called once per render-engine cycle from render-delegate-draw."
  (rs-sdl3:with-sdl3-event (ev)
    (loop while (rs-sdl3:poll-event ev)
          do (let ((clim-event (translate-sdl3-event port ev)))
               (when clim-event
                 (distribute-event port clim-event))))))

;;; Render delegate protocol implementation

(defmethod render-delegate-begin-frame ((delegate clim-render-delegate) target-time frame-number)
  "Called at start of frame - return layer tree (nil for immediate mode)."
  (declare (ignore target-time))
  (incf (delegate-frame-count delegate))
  ;; McCLIM uses immediate mode drawing, not retained layer trees
  nil)

(defmethod render-delegate-end-frame ((delegate clim-render-delegate) layer-tree frame-timings)
  "Called at end of frame."
  (declare (ignore layer-tree frame-timings)))

(defmethod render-delegate-notify-idle ((delegate clim-render-delegate) deadline)
  "Called when system is idle."
  (declare (ignore deadline)))

(defmethod render-delegate-draw ((delegate clim-render-delegate) pipeline-item)
  "Main drawing method - called by render engine each frame."
  (declare (ignore pipeline-item))
  (let ((surface (delegate-impeller-surface delegate))
        (port (delegate-port delegate)))
    ;; Belt-and-suspenders: drain events when drawing (main thread already does this in main-thread-loop)
    (drain-sdl3-events port)
    (when surface
      ;; Clear with background color
      (frs:with-display-list-builder (builder)
        ;; Store builder for medium drawing operations
        (setf (delegate-current-builder delegate) builder)
        
        (let ((bg (frs:make-paint)))
          (frs:paint-set-color bg 1.0 1.0 1.0 1.0)  ; White background
          (frs:draw-rect builder 0.0 0.0 
                          (float (rs-host:framebuffer-width (delegate-window delegate)) 1.0f0)
                          (float (rs-host:framebuffer-height (delegate-window delegate)) 1.0f0)
                          bg)
          (frs:release-paint bg))
        
        ;; TODO: Iterate over port's windows and draw their content
        ;; For now, just draw a test pattern
        (draw-test-pattern builder delegate)
        
        ;; Clear the builder reference before executing
        (setf (delegate-current-builder delegate) nil)
        
        (frs:execute-display-list surface builder))
      
      ;; Swap buffers
      (%sdl3:gl-swap-window (rs-sdl3::sdl3-window-handle (delegate-window delegate))))))

(defun get-delegate-current-builder (delegate)
  "Get the current display list builder for drawing, or nil if not in a frame."
  (when delegate
    (delegate-current-builder delegate)))

(defun draw-test-pattern (builder delegate)
  "Draw a simple test pattern to verify rendering works."
  (let ((paint (frs:make-paint)))
    (unwind-protect
         (progn
           ;; Draw a blue rectangle
           (frs:paint-set-color paint 0.2 0.4 0.9 1.0)
           (frs:draw-rect builder 50.0 50.0 200.0 150.0 paint)
           
           ;; Draw a red circle
           (frs:paint-set-color paint 0.9 0.2 0.2 1.0)
           (frs:draw-oval builder 300.0 50.0 100.0 100.0 paint)
           
           ;; Draw some text
           (frs:paint-set-color paint 0.0 0.0 0.0 1.0)
           ;; Text drawing would require typography context - skip for now
           )
      (frs:release-paint paint))))

;;; Cleanup

(defmethod destroy-delegate ((delegate clim-render-delegate))
  "Clean up Impeller resources."
  (when (delegate-impeller-surface delegate)
    (frs:release-surface (delegate-impeller-surface delegate))
    (setf (delegate-impeller-surface delegate) nil))
  (when (delegate-impeller-context delegate)
    (frs:release-context (delegate-impeller-context delegate))
    (setf (delegate-impeller-context delegate) nil)))
