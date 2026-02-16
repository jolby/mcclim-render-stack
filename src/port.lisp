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
                   :documentation "Current keyboard modifier state.")
   (window-table :accessor port-window-table
                 :initform (make-hash-table :test 'eq)
                 :documentation "SDL3 window → CLIM sheet mapping for event dispatch."))
  (:documentation "McCLIM port using render-stack (SDL3 + Impeller)."))

;;; ============================================================================
;;; Port Initialization
;;; ============================================================================

(defun %init-port-on-main-thread (port)
  "Initialize SDL3 and create the host on the main thread.
   Window, delegate, and engine are created later in realize-mirror."
  ;; Initialize SDL3 video
  (rs-sdl3:init-sdl3-video)
  ;; Create host (no window yet — deferred to realize-mirror)
  (setf (port-host port) (make-instance 'rs-sdl3:sdl3-host)))

(defmethod initialize-instance :after ((port render-stack-port) &key)
  "Initialize SDL3 and create the host. Window/engine deferred to realize-mirror."
  ;; Initialize SDL3 video subsystem
  (rs-internals:register-main-thread)
  (rs-internals:ensure-tmt-runner-ready)
  (trivial-main-thread:call-in-main-thread
   (lambda () (%init-port-on-main-thread port)))

  ;; Create typography context for text rendering
  (setf (port-typography-context port) (frs:make-typography-context))

  ;; Initialize event queue
  (setf (port-event-queue port) (make-array 0 :adjustable t :fill-pointer 0))
  (setf (port-event-queue-lock port) (bt2:make-lock "event-queue-lock"))
  (setf (port-event-queue-condition port) (bt2:make-condition-variable)))

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

    ;; Keypad - these constants may not be available in SDL3 bindings
    ;; (#.%sdl3:+k-kp-enter+ :kp-enter)
    ;; (#.%sdl3:+k-kp-multiply+ :kp-multiply)
    ;; (#.%sdl3:+k-kp-add+ :kp-add)
    ;; etc.

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
       (when (port-delegate port)
         (update-delegate-surface (port-delegate port)))
       ;; Get new size from window
       (let ((width (rs-host:window-width window))
             (height (rs-host:window-height window)))
         (make-instance 'window-configuration-event
                        :sheet sheet
                        :x 0 :y 0
                        :width width
                        :height height)))

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

(defun port-find-sheet-for-window (port window)
  "Look up the CLIM sheet associated with an SDL3 window."
  (gethash window (port-window-table port)))

(defmethod port-keyboard-input-focus ((port render-stack-port))
  "Return the sheet with keyboard focus."
  (let ((window (port-window port)))
    (when window
      (port-find-sheet-for-window port window))))

(defmethod port-pointer :before ((port render-stack-port))
  "Ensure the pointer is created when accessed."
  (unless (slot-value port 'pointer)
    (setf (slot-value port 'pointer)
          (make-instance 'render-stack-pointer :port port))))

;;; Main thread loop function (stub for test compatibility)
(defun main-thread-loop (port)
  "Main thread event+render loop stub."
  (declare (ignore port)))
