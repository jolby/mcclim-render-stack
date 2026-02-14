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
                       :documentation "Impeller typography context for text rendering."))
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

    ;; Start event processing thread
    (start-event-thread port)))

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
  "Process SDL3 events and dispatch to CLIM."
  (rs-sdl3:with-sdl3-event (ev)
    (loop until (port-quit-requested port)
          do (loop while (rs-sdl3:poll-event ev)
                   do (handle-sdl3-event port ev))
             (sleep 0.001))))  ; Small sleep to prevent busy-waiting

(defun handle-sdl3-event (port event)
  "Handle a single SDL3 event."
  (let ((event-type (rs-sdl3:get-event-type event)))
    (case event-type
      (:quit
       (setf (port-quit-requested port) t)
       ;; Notify CLIM to quit
       (dispatch-event port (make-instance 'window-destroy-event
                                           :sheet (port-window port))))
      (:window-close-requested
       (setf (port-quit-requested port) t))
      (:key-down
       (let ((keycode (rs-sdl3:keyboard-event-keycode event)))
         (when (= keycode %sdl3:+k-escape+)
           (setf (port-quit-requested port) t))))
      (:window-resized
       ;; Window was resized - update surface
       (when (port-delegate port)
         (update-delegate-surface (port-delegate port))))
      ;; TODO: Handle more event types
      )))

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
  "Process the next event (called by McCLIM's event loop)."
  (declare (ignore wait-function timeout))
  ;; Events are handled in the separate event thread
  ;; This method is called by McCLIM's main loop
  (sleep 0.01)  ; Yield to prevent busy-waiting
  (not (port-quit-requested port)))

(defmethod get-next-event ((port render-stack-port) &key wait-function timeout)
  "Get the next event from the queue."
  (declare (ignore wait-function timeout))
  ;; TODO: Implement proper event queue
  nil)

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

(defmethod port-pointer ((port render-stack-port))
  "Return the port's pointer."
  ;; TODO: Implement pointer handling
  nil)
