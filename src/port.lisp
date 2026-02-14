;;;; port.lisp — McCLIM Port using render-stack (SDL3 + Impeller)
;;;;
;;;; The port is the top-level connection to the display server.
;;;; This implementation uses SDL3 for windowing and events,
;;;; and Flutter Impeller (via render-stack) for rendering.

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; Port Class
;;; ============================================================================

(defclass render-stack-port (basic-port)
  ((host :accessor port-host :initform nil)
   (window :accessor port-window :initform nil)
   (engine :accessor port-engine :initform nil)
   (delegate :accessor port-delegate :initform nil)
   (gl-context :accessor port-gl-context :initform nil)
   (event-thread :accessor port-event-thread :initform nil)
   (quit-requested :accessor port-quit-requested :initform nil)
   (typography-context :accessor port-typography-context :initform nil
                       :documentation "Impeller typography context for text rendering.")
   (event-queue :accessor port-event-queue :initform nil
                :documentation "Thread-safe queue for CLIM events.")
   (event-queue-lock :accessor port-event-queue-lock :initform nil
                    :documentation "Lock for synchronizing access to event queue.")
   (event-queue-condition :accessor port-event-queue-condition :initform nil
                         :documentation "Condition variable for blocking on empty queue.")
   (modifier-state :accessor port-modifier-state :initform 0
                   :documentation "Current keyboard modifier state."))
  (:documentation "McCLIM port using render-stack (SDL3 + Impeller)."))

;;; ============================================================================
;;; Port Initialization
;;; ============================================================================

(defmethod initialize-instance :after ((port render-stack-port) &key)
  "Initialize SDL3 and create the render-stack engine."
  ;; Initialize SDL3 video subsystem
  (rs-sdl3:init-sdl3-video)
  
  ;; Create SDL3 host
  (setf (port-host port) (make-instance 'rs-sdl3:sdl3-host))
  
  ;; Set GL attributes before window creation
  (%sdl3:gl-set-attribute 5 1)  ; Double buffer
  
  ;; Create main window
  (let ((window (rs-host:make-window (port-host port)
                                      :title "McCLIM on Render Stack"
                                      :width 800
                                      :height 600)))
    (setf (port-window port) window)
    
    ;; Make GL context current
    (setf (port-gl-context port) (rs-host:window-graphics-context window))
    (%sdl3:gl-make-current (rs-sdl3::sdl3-window-handle window)
                           (port-gl-context port))
    (%sdl3:gl-set-swap-interval 1)  ; Enable vsync
    
    ;; Create render delegate
    (setf (port-delegate port) 
          (make-instance 'clim-render-delegate
                         :port port
                         :window window))
    
    ;; Create and start render engine
    (setf (port-engine port)
          (make-render-engine :delegate (port-delegate port)
                              :pipeline-depth 2
                              :target-fps 60))
    
    ;; Register main thread (should already be main, but just in case)
    (rs-internals:register-main-thread)

    ;; Start the engine
    (render-engine-start (port-engine port))

    ;; Create typography context for text rendering
    (setf (port-typography-context port) (frs:make-typography-context))

    ;; Initialize event queue
    (setf (port-event-queue port) (make-array 0 :adjustable t :fill-pointer 0))
    (setf (port-event-queue-lock port) (bt2:make-lock "event-queue-lock"))
    (setf (port-event-queue-condition port) (bt2:make-condition-variable))

    ;; Create pointer object
    (setf (port-pointer port) (make-instance 'render-stack-pointer :port port))

    ;; Start event processing thread
    (start-event-thread port)))

;;; ============================================================================
;;; Event Queue Operations
;;; ============================================================================

(defun port-enqueue-event (port event)
  "Enqueue a CLIM event to the port's event queue.
   Thread-safe: can be called from any thread."
  (bt2:with-lock-held ((port-event-queue-lock port))
    (vector-push-extend event (port-event-queue port))
    (bt2:condition-notify (port-event-queue-condition port))))

(defun port-dequeue-event (port &optional timeout)
  "Dequeue a CLIM event from the port's event queue.
   If the queue is empty, blocks until an event arrives or timeout expires.
   Returns the event, or NIL if timeout expired."
  (bt2:with-lock-held ((port-event-queue-lock port))
    (loop
      ;; Check if there's an event
      (when (> (fill-pointer (port-event-queue port)) 0)
        ;; Remove and return first event
        (let ((event (aref (port-event-queue port) 0)))
          ;; Shift remaining events down
          (replace (port-event-queue port) (port-event-queue port)
                   :start1 0 :start2 1)
          (decf (fill-pointer (port-event-queue port)))
          (return event)))
      ;; No event - wait for one
      (if timeout
          (unless (bt2:condition-wait (port-event-queue-condition port)
                                      (port-event-queue-lock port)
                                      :timeout timeout)
            (return nil))  ; Timeout expired
          (bt2:condition-wait (port-event-queue-condition port)
                              (port-event-queue-lock port))))))

(defun port-peek-event (port)
  "Return the next event without removing it, or NIL if queue is empty.
   Thread-safe: can be called from any thread."
  (bt2:with-lock-held ((port-event-queue-lock port))
    (when (> (fill-pointer (port-event-queue port)) 0)
      (aref (port-event-queue port) 0))))

(defun port-event-queue-empty-p (port)
  "Return T if the event queue is empty."
  (bt2:with-lock-held ((port-event-queue-lock port))
    (= (fill-pointer (port-event-queue port)) 0)))

;;; ============================================================================
;;; Keyboard Event Translation
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

    ;; Keypad
    (#.%sdl3:+k-kp-enter+ :kp-enter)
    (#.%sdl3:+k-kp-multiply+ :kp-multiply)
    (#.%sdl3:+k-kp-add+ :kp-add)
    (#.%sdl3:+k-kp-subtract+ :kp-subtract)
    (#.%sdl3:+k-kp-decimal+ :kp-decimal)
    (#.%sdl3:+k-kp-divide+ :kp-divide)
    (#.%sdl3:+k-kp-0+ :kp-0)
    (#.%sdl3:+k-kp-1+ :kp-1)
    (#.%sdl3:+k-kp-2+ :kp-2)
    (#.%sdl3:+k-kp-3+ :kp-3)
    (#.%sdl3:+k-kp-4+ :kp-4)
    (#.%sdl3:+k-kp-5+ :kp-5)
    (#.%sdl3:+k-kp-6+ :kp-6)
    (#.%sdl3:+k-kp-7+ :kp-7)
    (#.%sdl3:+k-kp-8+ :kp-8)
    (#.%sdl3:+k-kp-9+ :kp-9)

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

;;; ============================================================================
;;; Event Processing
;;; ============================================================================

(defun start-event-thread (port)
  "Start a thread to process SDL3 events."
  (setf (port-event-thread port)
        (bt2:make-thread 
         (lambda () (event-loop port))
         :name "mcclim-render-stack-events")))

(defun event-loop (port)
  "Process SDL3 events and enqueue CLIM events for later dispatch.
   The SDL3 event thread polls SDL and translates events to CLIM format,
   then enqueues them. McCLIM's main thread dequeues via process-next-event."
  (rs-sdl3:with-sdl3-event (ev)
    (loop until (port-quit-requested port)
          do (loop while (rs-sdl3:poll-event ev)
                   do (let ((clim-event (translate-sdl3-event port ev)))
                        (when clim-event
                          (port-enqueue-event port clim-event))))
               (sleep 0.001))))  ; Small sleep to prevent busy-waiting

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
       ;; Window was resized - update surface
       (when (port-delegate port)
         (update-delegate-surface (port-delegate port)))
       ;; TODO: Create window-configuration-event with new size
       nil)

      (:key-down
        (let ((keycode (rs-sdl3:keyboard-event-keycode event))
              (modifiers (rs-sdl3:keyboard-event-modifiers event))
              (x (rs-sdl3:keyboard-event-x event))
              (y (rs-sdl3:keyboard-event-y event)))
          ;; Handle escape as quit
          (when (= keycode %sdl3:+k-escape+)
            (setf (port-quit-requested port) t))
          ;; Create key press event
          (let ((key-name (sdl3-keycode-to-key-name keycode))
                (mod-state (sdl3-modifiers-to-clim modifiers)))
            (when key-name
              (make-instance 'key-press-event
                            :sheet (port-window port)
                            :key-name key-name
                            :key-character (sdl3-keycode-to-character keycode mod-state)
                            :x (or x 0)
                            :y (or y 0)
                            :modifier-state mod-state)))))

      (:key-up
        (let ((keycode (rs-sdl3:keyboard-event-keycode event))
              (modifiers (rs-sdl3:keyboard-event-modifiers event))
              (x (rs-sdl3:keyboard-event-x event))
              (y (rs-sdl3:keyboard-event-y event)))
          (let ((key-name (sdl3-keycode-to-key-name keycode))
                (mod-state (sdl3-modifiers-to-clim modifiers)))
            (when key-name
              (make-instance 'key-release-event
                            :sheet (port-window port)
                            :key-name key-name
                            :key-character (sdl3-keycode-to-character keycode mod-state)
                            :x (or x 0)
                            :y (or y 0)
                            :modifier-state mod-state)))))

      (:mouse-button-down
       ;; TODO: Return pointer-button-press-event in bd-7eh.18
       nil)

      (:mouse-button-up
       ;; TODO: Return pointer-button-release-event in bd-7eh.18
       nil)

      (:mouse-motion
       ;; TODO: Return pointer-motion-event in bd-7eh.19
       nil)

      (:mouse-wheel
       ;; TODO: Return pointer-scroll-event in bd-7eh.19
       nil)

      ;; Ignore unhandled events for now
      (t nil))))

;;; ============================================================================
;;; Port Protocol Methods
;;; ============================================================================

(defmethod destroy-port ((port render-stack-port))
  "Clean up all resources."
  ;; Signal quit
  (setf (port-quit-requested port) t)
  
  ;; Wait for event thread
  (when (port-event-thread port)
    (bt2:join-thread (port-event-thread port))
    (setf (port-event-thread port) nil))
  
  ;; Stop and destroy engine
  (when (port-engine port)
    (render-engine-stop (port-engine port))
    (setf (port-engine port) nil))
  
  ;; Clean up delegate
  (when (port-delegate port)
    (destroy-delegate (port-delegate port))
    (setf (port-delegate port) nil))

  ;; Release typography context
  (when (port-typography-context port)
    (frs:release-typography-context (port-typography-context port))
    (setf (port-typography-context port) nil))

  ;; Destroy window
  (when (port-window port)
    (rs-host:destroy-window (port-host port) (port-window port))
    (setf (port-window port) nil))
  
  ;; Quit SDL3
  (rs-sdl3:quit-sdl3))

(defmethod process-next-event ((port render-stack-port) &key wait-function timeout)
  "Process the next event from the queue (called by McCLIM's main loop).

   This is the main integration point between SDL3 events and McCLIM.
   The SDL3 event thread enqueues events, and this method dequeues and dispatches them.

   Parameters:
     wait-function - called periodically to check if we should continue waiting
     timeout - maximum time to wait for an event (in seconds)

   Returns T if events may still be available, NIL if port is shutting down."
  ;; Check if we should quit
  (when (port-quit-requested port)
    (return-from process-next-event nil))

  ;; Try to get an event from the queue
  (let ((event (port-dequeue-event port (or timeout 0.01))))
    (when event
      ;; Dispatch the event to its sheet
      (dispatch-event port event))

    ;; Call wait-function if provided
    (when wait-function
      (unless (funcall wait-function)
        (return-from process-next-event nil)))

    ;; Return T to indicate we should be called again
    (not (port-quit-requested port))))

(defmethod get-next-event ((port render-stack-port) &key wait-function timeout)
  "Get the next event from the queue without dispatching it.

   Parameters:
     wait-function - called periodically to check if we should continue waiting
     timeout - maximum time to wait for an event (in seconds)

   Returns the event, or NIL if no event available."
  ;; Call wait-function if provided
  (when wait-function
    (unless (funcall wait-function)
      (return-from get-next-event nil)))

  ;; Try to get an event from the queue
  (port-dequeue-event port timeout))

(defmethod port-force-output ((port render-stack-port))
  "Flush pending drawing operations."
  ;; The render engine handles this automatically
  )

(defmethod port-set-mirror-region ((port render-stack-port) mirror region)
  "Set the mirror's visible region."
  (declare (ignore mirror region)))

(defmethod port-set-mirror-transformation ((port render-stack-port) mirror transformation)
  "Set the mirror's transformation."
  (declare (ignore mirror transformation)))

;;; ============================================================================
;;; Port Capabilities
;;; ============================================================================

(defmethod port-keyboard-input-focus ((port render-stack-port))
  "Return the sheet with keyboard focus."
  (port-window port))
