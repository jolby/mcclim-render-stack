;;;; events.lisp -- Tier 2b: synthetic SDL3 event injection
;;;;
;;;; Provides REPL-callable helpers to inject mouse events into the running
;;;; SDL3 event queue.  SDL_PushEvent is thread-safe; these can be called from
;;;; any thread.  Events are processed on the next runner iteration's event-
;;;; drain phase -- so inject first, then advance-frames to consume + render.
;;;;
;;;; Window ID lookup uses climi::*all-ports* to find the active render-stack
;;;; port without requiring the user to thread port references around.
;;;;
;;;; Button coordinates are in LOGICAL window pixels (same coordinate system
;;;; as CLIM pointer events), matching the values you see in CLIM handlers.

(in-package :mcclim-render-stack/test-rig)

;;; ============================================================================
;;; Window ID helpers
;;; ============================================================================

(defun %find-render-stack-port ()
  "Find the active render-stack port via climi::*all-ports*, or NIL."
  (find 'mcclim-render-stack:render-stack-port
        climi::*all-ports*
        :key #'type-of))

(defun get-window-ids ()
  "Return a list of SDL3 window IDs for all currently registered mirrors.
Useful for passing an explicit WINDOW-ID to the inject-* functions when
there are multiple windows."
  (let ((port (%find-render-stack-port)))
    (when port
      (mapcar #'mcclim-render-stack:mirror-window-id
              (mcclim-render-stack:collect-registered-mirrors port)))))

(defun %require-window-id (window-id)
  "Return WINDOW-ID if non-nil, else auto-detect from the registered mirrors.
Signals an error if no window ID can be determined."
  (or window-id
      (first (get-window-ids))
      (error "No window-id provided and none found. Is the demo running?")))

;;; ============================================================================
;;; SDL3 event type constants
;;;
;;; SDL_EventType is an ENHANCED-TYPEDEF in claw bindings; cffi's keyword->enum
;;; lookup (KEYWORD-VALUES) is not implemented for it.  Write the type field
;;; directly as :uint32 using these SDL3 integer values.
;;; ============================================================================

(defconstant +sdl-event-mouse-motion+      #x400)  ; SDL_EVENT_MOUSE_MOTION
(defconstant +sdl-event-mouse-button-down+ #x401)  ; SDL_EVENT_MOUSE_BUTTON_DOWN
(defconstant +sdl-event-mouse-button-up+   #x402)  ; SDL_EVENT_MOUSE_BUTTON_UP

;;; ============================================================================
;;; Low-level helpers
;;; ============================================================================

(defun %zero-foreign-object (ptr byte-count)
  "Zero BYTE-COUNT bytes at PTR."
  (dotimes (i byte-count)
    (setf (cffi:mem-aref ptr :uint8 i) 0)))

(defun %set-event-type (struct-ptr struct-type type-int)
  "Write TYPE-INT into the 'type' slot of STRUCT-PTR as :uint32.
Bypasses claw's ENHANCED-TYPEDEF enum wrapper which lacks KEYWORD-VALUES."
  (setf (cffi:mem-ref
         (cffi:foreign-slot-pointer struct-ptr struct-type '%sdl3::type)
         :uint32)
        type-int))

;;; ============================================================================
;;; Mouse motion
;;; ============================================================================

(defun inject-mouse-move (x y &key window-id)
  "Inject a synthetic SDL_MOUSEMOTION event at logical window coordinates (X, Y).
WINDOW-ID defaults to the first registered mirror.
Thread-safe: SDL_PushEvent is thread-safe per SDL3 docs."
  (let ((wid (%require-window-id window-id)))
    (cffi:with-foreign-object (ev '(:union %sdl3::event))
      (%zero-foreign-object ev (cffi:foreign-type-size '(:union %sdl3::event)))
      (let ((motion (cffi:foreign-slot-pointer ev '(:union %sdl3::event) '%sdl3::motion)))
        (%set-event-type motion '(:struct %sdl3::mouse-motion-event) +sdl-event-mouse-motion+)
        (setf (cffi:foreign-slot-value motion '(:struct %sdl3::mouse-motion-event) '%sdl3::window-id)
              wid)
        (setf (cffi:foreign-slot-value motion '(:struct %sdl3::mouse-motion-event) '%sdl3::x)
              (float x 1.0f0))
        (setf (cffi:foreign-slot-value motion '(:struct %sdl3::mouse-motion-event) '%sdl3::y)
              (float y 1.0f0)))
      (sdl3-ffi:push-event ev))))

;;; ============================================================================
;;; Mouse buttons
;;; ============================================================================

(defun inject-mouse-down (x y &key (button 1) window-id)
  "Inject a synthetic SDL_MOUSEBUTTONDOWN at logical coordinates (X, Y).
BUTTON: 1=left (default), 2=middle, 3=right.
WINDOW-ID defaults to the first registered mirror."
  (let ((wid (%require-window-id window-id)))
    (cffi:with-foreign-object (ev '(:union %sdl3::event))
      (%zero-foreign-object ev (cffi:foreign-type-size '(:union %sdl3::event)))
      (let ((btn (cffi:foreign-slot-pointer ev '(:union %sdl3::event) '%sdl3::button)))
        (%set-event-type btn '(:struct %sdl3::mouse-button-event) +sdl-event-mouse-button-down+)
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::window-id)
              wid)
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::button)
              button)
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::down)
              t)
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::clicks)
              1)
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::x)
              (float x 1.0f0))
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::y)
              (float y 1.0f0)))
      (sdl3-ffi:push-event ev))))

(defun inject-mouse-up (x y &key (button 1) window-id)
  "Inject a synthetic SDL_MOUSEBUTTONUP at logical coordinates (X, Y).
BUTTON: 1=left (default), 2=middle, 3=right.
WINDOW-ID defaults to the first registered mirror."
  (let ((wid (%require-window-id window-id)))
    (cffi:with-foreign-object (ev '(:union %sdl3::event))
      (%zero-foreign-object ev (cffi:foreign-type-size '(:union %sdl3::event)))
      (let ((btn (cffi:foreign-slot-pointer ev '(:union %sdl3::event) '%sdl3::button)))
        (%set-event-type btn '(:struct %sdl3::mouse-button-event) +sdl-event-mouse-button-up+)
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::window-id)
              wid)
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::button)
              button)
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::down)
              nil)
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::clicks)
              1)
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::x)
              (float x 1.0f0))
        (setf (cffi:foreign-slot-value btn '(:struct %sdl3::mouse-button-event) '%sdl3::y)
              (float y 1.0f0)))
      (sdl3-ffi:push-event ev))))

;;; ============================================================================
;;; Convenience: full click
;;; ============================================================================

(defun inject-click (x y &key (button 1) window-id)
  "Inject a mouse-down then mouse-up at (X, Y) -- a complete click.
Does NOT advance frames; call advance-frames after to process and render."
  (inject-mouse-down x y :button button :window-id window-id)
  (inject-mouse-up   x y :button button :window-id window-id))
