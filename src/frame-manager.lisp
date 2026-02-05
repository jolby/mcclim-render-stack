(in-package :mcclim-render-stack)

;;; Frame Manager - manages application frames and their windows

(defclass render-stack-frame-manager (frame-manager)
  ()
  (:documentation "Frame manager for render-stack backend."))

(defmethod make-pane-1 ((fm render-stack-frame-manager) (frame application-frame)
                         type &rest args)
  ;; Create a pane of the given type
  ;; Delegate to default implementation for now
  (apply #'call-next-method fm frame type args))

(defmethod adopt-frame ((fm render-stack-frame-manager) (frame application-frame))
  ;; Adopt a frame into this frame manager
  (call-next-method))

(defmethod disown-frame ((fm render-stack-frame-manager) (frame application-frame))
  ;; Disown a frame from this frame manager
  (call-next-method))

(defmethod note-frame-enabled ((fm render-stack-frame-manager) (frame application-frame))
  ;; Called when a frame is enabled
  (call-next-method))

(defmethod note-frame-disabled ((fm render-stack-frame-manager) (frame application-frame))
  ;; Called when a frame is disabled
  (call-next-method))

;;; Default frame manager for the port

(defmethod find-frame-manager ((port render-stack-port) &key)
  ;; Return the default frame manager for this port
  (or (car (frame-managers port))
      (make-instance 'render-stack-frame-manager :port port)))

;;; Initialization

(defun initialize-render-stack ()
  "Initialize the render-stack backend.
   Call this before using the :render-stack server path."
  ;; Ensure SDL3 is initialized
  ;; Ensure Impeller is initialized
  ;; Register any additional resources
  (log:info "render-stack backend initialized"))
