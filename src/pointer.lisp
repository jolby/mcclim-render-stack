;;;; pointer.lisp — Pointer (mouse) handling for McCLIM render-stack backend

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; Pointer Class
;;; ============================================================================

(defclass render-stack-pointer (standard-pointer)
  ((button-state :accessor pointer-button-state-cache :initform climi::+pointer-no-button+
                 :documentation "Cached button state for the pointer.")
   (last-x :accessor pointer-last-x :initform 0 :type real
           :documentation "Last known X position.")
   (last-y :accessor pointer-last-y :initform 0 :type real
           :documentation "Last known Y position."))
  (:documentation "Pointer class for the render-stack backend.
                   Represents the mouse pointer device."))

;;; ============================================================================
;;; Cursor Management
;;; ============================================================================

(defun map-clim-cursor-to-sdl3 (cursor-name)
  "Map CLIM cursor keyword to SDL3 system cursor enum value.
   Returns the SDL3 cursor enum or :default if unmapped."
  (case cursor-name
    ((:default :arrow) :default)
    ((:prompt :text :i-beam) :text)
    ((:button :hand :pointer :grab) :pointer)
    ((:busy :wait) :wait)
    ((:not-allowed :forbidden) :not-allowed)
    ((:move) :move)
    ((:arrow-we) :ew-resize)
    ((:arrow-ns) :ns-resize)
    ((:arrow-nwse) :nwse-resize)
    ((:arrow-nesw) :nesw-resize)
    (otherwise :default)))

(defun set-sdl3-system-cursor (cursor-enum)
  "Create and set SDL3 system cursor.
   CURSOR-ENUM should be an SDL3 system cursor enum value (keyword)."
  (let ((cursor (rs-sdl3:create-system-cursor cursor-enum)))
    (unless (cffi:null-pointer-p cursor)
      (rs-sdl3:set-sdl3-cursor cursor)
      t)))

(defmethod (setf climi::pointer-cursor)
    ((design symbol) (pointer render-stack-pointer))
  "Set pointer cursor from CLIM cursor keyword."
  (let ((sdl3-cursor (map-clim-cursor-to-sdl3 design)))
    (set-sdl3-system-cursor sdl3-cursor)))

;;; ============================================================================
;;; Pointer Position Tracking
;;; ============================================================================

(defmethod pointer-position ((pointer render-stack-pointer))
  "Return the current pointer position as two values: x, y."
  (values (pointer-last-x pointer)
          (pointer-last-y pointer)))

(defun update-pointer-position (pointer x y)
  "Update the cached pointer position."
  (setf (pointer-last-x pointer) x
        (pointer-last-y pointer) y))

;;; ============================================================================
;;; Pointer Button State
;;; ============================================================================

(defmethod pointer-button-state ((pointer render-stack-pointer))
  "Return the current pointer button state."
  (pointer-button-state-cache pointer))

(defun update-pointer-button-state (pointer button pressed)
  "Update the pointer button state.
   BUTTON is a CLIM pointer button constant (:left, :middle, :right, etc.)
   PRESSED is T if the button was pressed, NIL if released."
  (let ((current-state (pointer-button-state-cache pointer)))
    (if pressed
        (setf (pointer-button-state-cache pointer)
              (logior current-state
                      (case button
                        (:left +pointer-left-button+)
                        (:middle +pointer-middle-button+)
                        (:right +pointer-right-button+)
                        (:wheel-up +pointer-wheel-up+)
                        (:wheel-down +pointer-wheel-down+)
                        (:wheel-left +pointer-wheel-left+)
                        (:wheel-right +pointer-wheel-right+)
                        (t 0))))
        (setf (pointer-button-state-cache pointer)
              (logandc1 (case button
                          (:left +pointer-left-button+)
                          (:middle +pointer-middle-button+)
                          (:right +pointer-right-button+)
                          (:wheel-up +pointer-wheel-up+)
                          (:wheel-down +pointer-wheel-down+)
                          (:wheel-left +pointer-wheel-left+)
                          (:wheel-right +pointer-wheel-right+)
                          (t 0))
                        current-state)))))

;;; ============================================================================
;;; SDL3 Button to CLIM Button Mapping
;;; ============================================================================

(defun sdl3-button-to-clim-button (sdl3-button)
  "Convert SDL3 mouse button ID to CLIM button keyword.
   SDL3 buttons: 1=left, 2=middle, 3=right, 4=wheel-up, 5=wheel-down"
  (case sdl3-button
    (1 :left)
    (2 :middle)
    (3 :right)
    (4 :wheel-up)
    (5 :wheel-down)
    (t nil)))

(defun sdl3-button-to-clim-constant (sdl3-button)
  "Convert SDL3 mouse button ID to CLIM pointer button constant.
   Returns the actual +pointer-*-button+ constant value."
  (case sdl3-button
    (1 +pointer-left-button+)
    (2 +pointer-middle-button+)
    (3 +pointer-right-button+)
    (4 +pointer-wheel-up+)
    (5 +pointer-wheel-down+)
    (t climi::+pointer-no-button+)))

;;; ============================================================================
;;; SDL3 Modifier to CLIM Modifier Conversion
;;; ============================================================================

(defun sdl3-mod-to-clim-mod (sdl3-mod)
  "Convert SDL3 modifier bitmask to CLIM modifier state."
  (let ((mod 0))
    (when (logtest sdl3-mod 1)   ; KMOD_LSHIFT
      (setf mod (logior mod +shift-key+)))
    (when (logtest sdl3-mod 2)   ; KMOD_RSHIFT
      (setf mod (logior mod +shift-key+)))
    (when (logtest sdl3-mod 4)   ; KMOD_LCTRL
      (setf mod (logior mod +control-key+)))
    (when (logtest sdl3-mod 8)   ; KMOD_RCTRL
      (setf mod (logior mod +control-key+)))
    (when (logtest sdl3-mod 16)  ; KMOD_LALT
      (setf mod (logior mod +meta-key+)))
    (when (logtest sdl3-mod 32)  ; KMOD_RALT
      (setf mod (logior mod +meta-key+)))
    (when (logtest sdl3-mod 64)  ; KMOD_LGUI (Super/Windows)
      (setf mod (logior mod +super-key+)))
    (when (logtest sdl3-mod 128) ; KMOD_RGUI
      (setf mod (logior mod +super-key+)))
    (when (logtest sdl3-mod 256) ; KMOD_NUM
      (setf mod (logior mod +num-lock-key+)))
    (when (logtest sdl3-mod 512) ; KMOD_CAPS
      (setf mod (logior mod +lock-key+)))
    mod))
