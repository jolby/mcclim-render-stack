;;;; events.lisp — SDL3 → CLIM event translation
;;;;
;;;; Defines:
;;;;   get-window-id-from-sdl3-event  — extract window-id from raw SDL3 event
;;;;   sdl3-keycode-to-clim-key-name  — SDL3 keycode → CLIM key-name keyword
;;;;   sdl3-keycode-to-character      — SDL3 keycode → printable character or nil
;;;;   translate-sdl3-event           — full SDL3 event → CLIM event dispatch

(in-package :mcclim-render-stack)

;;; SDL3 modifier key constants
;;; These are bitmask values used in keyboard event mod field

(defconstant +sdl3-kmod-lshift+ 1)
(defconstant +sdl3-kmod-rshift+ 2)
(defconstant +sdl3-kmod-lctrl+ 64)
(defconstant +sdl3-kmod-rctrl+ 128)
(defconstant +sdl3-kmod-lalt+ 256)
(defconstant +sdl3-kmod-ralt+ 512)
(defconstant +sdl3-kmod-lgui+ 1024)
(defconstant +sdl3-kmod-rgui+ 2048)

(defconstant +sdl3-kmod-shift+ 3)
(defconstant +sdl3-kmod-ctrl+ 192)
(defconstant +sdl3-kmod-alt+ 768)
(defconstant +sdl3-kmod-gui+ 3072)

;;; SDL3 mouse button constants
(defconstant +sdl3-button-left+ 1)
(defconstant +sdl3-button-middle+ 2)
(defconstant +sdl3-button-right+ 3)
(defconstant +sdl3-button-x1+ 4)
(defconstant +sdl3-button-x2+ 5)

;; Forward declarations for functions defined in mirror.lisp (loaded after events.lisp).
;; These are resolved at runtime; the declaims suppress SBCL compile-time warnings.
(declaim (ftype (function (t t) t) find-mirror-by-window-id))
(declaim (ftype (function (t) t) invalidate-mirror-surface))

;;; ============================================================================
;;; Window-id extraction
;;; ============================================================================

(defun get-window-id-from-sdl3-event (event-ptr event-type)
  "Return the SDL3 window-id (integer) from EVENT-PTR, or NIL for global events.
Dispatches on EVENT-TYPE keyword using the appropriate rs-sdl3 accessor."
  (case event-type
    ((:window-close-requested :window-exposed :window-resized
      :window-shown :window-hidden :window-moved
      :window-focus-gained :window-focus-lost
      :window-mouse-enter :window-mouse-leave)
     (rs-sdl3:window-event-window-id event-ptr))
    ((:key-down :key-up)
     (rs-sdl3:keyboard-event-window-id event-ptr))
    ((:mouse-button-down :mouse-button-up)
     (rs-sdl3:mouse-button-event-window-id event-ptr))
    (:mouse-motion
     (rs-sdl3:mouse-motion-event-window-id event-ptr))
    (:mouse-wheel
     (rs-sdl3:mouse-wheel-event-window-id event-ptr))
    (otherwise nil)))

;;; Modifier translation

(defun sdl3-mod-to-clim-mod (sdl3-mod)
  "Translate SDL3 modifier bitmask to CLIM modifier bitmask.

  Arguments:
    sdl3-mod - SDL3 modifier state (integer bitmask from keyboard event)

  Returns:
    CLIM modifier state (integer bitmask using +shift-key+, etc.)"
  (declare (type integer sdl3-mod))
  (flet ((maybe-mod (clim-mod sdl3-mask)
           (if (plusp (logand sdl3-mod sdl3-mask))
               clim-mod
               0)))
    (logior (maybe-mod clim:+shift-key+ +sdl3-kmod-shift+)
            (maybe-mod clim:+control-key+ +sdl3-kmod-ctrl+)
            (maybe-mod clim:+meta-key+ +sdl3-kmod-alt+)
            (maybe-mod clim:+super-key+ +sdl3-kmod-lgui+)
            (maybe-mod clim:+hyper-key+ +sdl3-kmod-rgui+))))

;;; Button translation

(defun sdl3-button-to-clim-button (sdl3-button)
  "Translate SDL3 mouse button number to CLIM button constant.

  Arguments:
    sdl3-button - SDL3 button number (1=left, 2=middle, 3=right, 4=x1, 5=x2)

  Returns:
    CLIM button constant (+pointer-left-button+, etc.)"
  (declare (type integer sdl3-button))
  (cond ((= sdl3-button +sdl3-button-left+) clim:+pointer-left-button+)
        ((= sdl3-button +sdl3-button-middle+) clim:+pointer-middle-button+)
        ((= sdl3-button +sdl3-button-right+) clim:+pointer-right-button+)
        ((= sdl3-button +sdl3-button-x1+) clim:+pointer-wheel-down+)
        ((= sdl3-button +sdl3-button-x2+) clim:+pointer-wheel-up+)
        (t climi::+pointer-no-button+)))

;;; ============================================================================
;;; Keycode translation
;;; ============================================================================

(defun sdl3-keycode-to-clim-key-name (keycode)
  "Translate SDL3 keycode integer to a CLIM key-name keyword.
Uses SDL3's SDL_GetKeyName for automatic coverage of all keys.
Examples: F1 → :F1, Page Up → :PAGE-UP, a → :A, Escape → :ESCAPE.
Returns :UNKNOWN if SDL3 cannot name the key."
  (let* ((ptr  (%sdl3:get-key-name keycode))
         (name (when (and ptr (not (cffi:null-pointer-p ptr)))
                 (cffi:foreign-string-to-lisp ptr))))
    (if (and name (> (length name) 0))
        (intern (substitute #\- #\Space (string-upcase name)) :keyword)
        :unknown)))

(defun sdl3-keycode-to-character (keycode modifier-state)
  "Return a character for KEYCODE if it represents printable ASCII (32–126), else NIL.
Applies char-upcase when shift is held and the character is alphabetic."
  (when (and (>= keycode 32) (<= keycode 126))
    (let ((char (code-char keycode)))
      (if (and (logtest modifier-state +shift-key+) (alpha-char-p char))
          (char-upcase char)
          char))))

;;; ============================================================================
;;; Full event translation
;;; ============================================================================

(defun translate-sdl3-event (port event-ptr)
  "Translate a raw SDL3 event into a CLIM event object, or NIL if not handled.

Looks up the mirror by window-id via find-mirror-by-window-id (defined in
mirror.lisp). Returns NIL if no mirror is found for the window.

Thread Contract: MUST be called on the main thread from drain-sdl3-events-for-port."
  (let* ((event-type (rs-sdl3:get-event-type event-ptr))
         (window-id  (get-window-id-from-sdl3-event event-ptr event-type))
         (mirror     (when window-id
                       (find-mirror-by-window-id port window-id))))
    (unless mirror
      (return-from translate-sdl3-event nil))
    (let ((sheet   (mirror-sheet mirror))
          (pointer (port-pointer port)))
      (case event-type

        (:window-close-requested
         (setf (port-quit-requested port) t)
         (make-instance 'window-manager-delete-event :sheet sheet))

        (:window-exposed
         (make-instance 'window-repaint-event
                        :sheet sheet
                        :region +everywhere+))

        (:window-resized
         (let ((width  (rs-sdl3:window-event-data1 event-ptr))
               (height (rs-sdl3:window-event-data2 event-ptr)))
           ;; Releases cached FBO surface; mirror-width/height updated by SDL3 query inside.
           (invalidate-mirror-surface mirror)
           (make-instance 'window-configuration-event
                          :sheet sheet
                          :region (make-bounding-rectangle 0 0 width height))))

        (:window-shown
         (make-instance 'window-map-event :sheet sheet))

        (:window-hidden
         (make-instance 'window-unmap-event :sheet sheet))

        (:window-focus-gained
         (make-instance 'window-manager-focus-event :sheet sheet))

        (:window-focus-lost
         nil)

        (:window-mouse-enter
         (multiple-value-bind (x y) (pointer-position pointer)
           (make-instance 'pointer-enter-event
                          :sheet sheet
                          :pointer pointer
                          :x x :y y
                          :modifier-state (port-modifier-state port))))

        (:window-mouse-leave
         (multiple-value-bind (x y) (pointer-position pointer)
           (make-instance 'pointer-exit-event
                          :sheet sheet
                          :pointer pointer
                          :x x :y y
                          :modifier-state (port-modifier-state port))))

        (:key-down
         (let* ((keycode   (rs-sdl3:keyboard-event-keycode event-ptr))
                (modifiers (rs-sdl3:keyboard-event-mod event-ptr))
                (clim-mod  (sdl3-mod-to-clim-mod modifiers))
                (key-name  (sdl3-keycode-to-clim-key-name keycode))
                (key-char  (sdl3-keycode-to-character keycode modifiers))
                (target    (port-keyboard-input-focus port)))
           (setf (port-modifier-state port) clim-mod)
           (when (eq key-name :escape)
             (setf (port-quit-requested port) t))
           (when target
             (make-instance 'key-press-event
                            :sheet target
                            :x (pointer-last-x pointer)
                            :y (pointer-last-y pointer)
                            :modifier-state clim-mod
                            :key-name key-name
                            :key-character key-char))))

        (:key-up
         (let* ((keycode   (rs-sdl3:keyboard-event-keycode event-ptr))
                (modifiers (rs-sdl3:keyboard-event-mod event-ptr))
                (clim-mod  (sdl3-mod-to-clim-mod modifiers))
                (key-name  (sdl3-keycode-to-clim-key-name keycode))
                (key-char  (sdl3-keycode-to-character keycode modifiers))
                (target    (port-keyboard-input-focus port)))
           (setf (port-modifier-state port) clim-mod)
           (when target
             (make-instance 'key-release-event
                            :sheet target
                            :x (pointer-last-x pointer)
                            :y (pointer-last-y pointer)
                            :modifier-state clim-mod
                            :key-name key-name
                            :key-character key-char))))

        (:mouse-button-down
         (let* ((sdl-button (rs-sdl3:mouse-button-event-button event-ptr))
                (x          (rs-sdl3:mouse-button-event-x event-ptr))
                (y          (rs-sdl3:mouse-button-event-y event-ptr))
                (button     (sdl3-button-to-clim-constant sdl-button)))
           (update-pointer-position pointer x y)
           (make-instance 'pointer-button-press-event
                          :sheet sheet
                          :pointer pointer
                          :x x :y y
                          :button button
                          :modifier-state (port-modifier-state port))))

        (:mouse-button-up
         (let* ((sdl-button (rs-sdl3:mouse-button-event-button event-ptr))
                (x          (rs-sdl3:mouse-button-event-x event-ptr))
                (y          (rs-sdl3:mouse-button-event-y event-ptr))
                (button     (sdl3-button-to-clim-constant sdl-button)))
           (update-pointer-position pointer x y)
           (make-instance 'pointer-button-release-event
                          :sheet sheet
                          :pointer pointer
                          :x x :y y
                          :button button
                          :modifier-state (port-modifier-state port))))

        (:mouse-motion
         (let ((x (rs-sdl3:mouse-motion-event-x event-ptr))
               (y (rs-sdl3:mouse-motion-event-y event-ptr)))
           (update-pointer-position pointer x y)
           (make-instance 'pointer-motion-event
                          :sheet sheet
                          :pointer pointer
                          :x x :y y
                          :modifier-state (port-modifier-state port))))

        (:mouse-wheel
         (let* ((delta-x   (rs-sdl3:mouse-wheel-event-x event-ptr))
                (delta-y   (rs-sdl3:mouse-wheel-event-y event-ptr))
                (button    (cond ((> delta-y 0) +pointer-wheel-up+)
                                 ((< delta-y 0) +pointer-wheel-down+)
                                 (t climi::+pointer-no-button+))))
           (multiple-value-bind (x y) (pointer-position pointer)
             (make-instance 'pointer-scroll-event
                            :sheet sheet
                            :pointer pointer
                            :x x :y y
                            :button button
                            :modifier-state (port-modifier-state port)
                            :delta-x delta-x
                            :delta-y delta-y))))

        (otherwise nil)))))
