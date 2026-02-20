;;;; keyboard-event-tests.lisp — Unit tests for keyboard event translation

(in-package :mcclim-render-stack-tests)

(define-test sdl3-modifiers-to-clim-key-name-exists
  :parent mcclim-render-stack-suite
  "Test that sdl3-modifiers-to-clim-key-name function exists."
  (true (fboundp 'mcclim-render-stack::sdl3-modifiers-to-clim-key-name)))

(define-test sdl3-keycode-to-clim-key-name-exists
  :parent mcclim-render-stack-suite
  "Test that sdl3-keycode-to-clim-key-name function exists."
  (true (fboundp 'mcclim-render-stack::sdl3-keycode-to-clim-key-name)))

(define-test sdl3-keycode-to-character-exists
  :parent mcclim-render-stack-suite
  "Test that sdl3-keycode-to-character function exists."
  (true (fboundp 'mcclim-render-stack::sdl3-keycode-to-character)))

(define-test sdl3-modifiers-no-modifiers
  :parent mcclim-render-stack-suite
  "Test sdl3-mod-to-clim-mod returns 0 with no modifiers."
  (is = 0 (mcclim-render-stack::sdl3-mod-to-clim-mod 0)))

(define-test sdl3-modifiers-shift
  :parent mcclim-render-stack-suite
  "Test sdl3-mod-to-clim-mod with shift modifier."
  (let ((result (mcclim-render-stack::sdl3-mod-to-clim-mod 1)))
    (true (plusp (logand result clim:+shift-key+)))))

(define-test sdl3-modifiers-control
  :parent mcclim-render-stack-suite
  "Test sdl3-mod-to-clim-mod with ctrl modifier."
  (let ((result (mcclim-render-stack::sdl3-mod-to-clim-mod 64)))
    (true (plusp (logand result clim:+control-key+)))))

(define-test sdl3-modifiers-shift-ctrl
  :parent mcclim-render-stack-suite
  "Test sdl3-mod-to-clim-mod with shift+ctrl modifiers."
  (let ((result (mcclim-render-stack::sdl3-mod-to-clim-mod 65)))
    (true (plusp (logand result clim:+shift-key+)))
    (true (plusp (logand result clim:+control-key+)))))

(define-test sdl3-keycode-escape
  :parent mcclim-render-stack-suite
  "Test sdl3-keycode-to-clim-key-name for escape key."
  (is eq :escape (mcclim-render-stack::sdl3-keycode-to-clim-key-name 27)))

(define-test sdl3-keycode-return
  :parent mcclim-render-stack-suite
  "Test sdl3-keycode-to-clim-key-name for return key."
  (is eq :return (mcclim-render-stack::sdl3-keycode-to-clim-key-name 13)))

(define-test sdl3-keycode-space
  :parent mcclim-render-stack-suite
  "Test sdl3-keycode-to-clim-key-name for space key."
  (is eq :space (mcclim-render-stack::sdl3-keycode-to-clim-key-name 32)))

(define-test sdl3-keycode-letter
  :parent mcclim-render-stack-suite
  "Test sdl3-keycode-to-clim-key-name for letter keys."
  (is = 65 (mcclim-render-stack::sdl3-keycode-to-clim-key-name 65)))

(define-test sdl3-keycode-digit
  :parent mcclim-render-stack-suite
  "Test sdl3-keycode-to-clim-key-name for digit keys."
  (is = 48 (mcclim-render-stack::sdl3-keycode-to-clim-key-name 48)))

(define-test sdl3-keycode-unknown
  :parent mcclim-render-stack-suite
  "Test sdl3-keycode-to-clim-key-name for unknown keycode returns numeric."
  (let ((result (mcclim-render-stack::sdl3-keycode-to-clim-key-name 9999)))
    (is = result 9967)))

(define-test translate-sdl3-event-exists
  :parent mcclim-render-stack-suite
  "Test that translate-sdl3-event function exists."
  (true (fboundp 'mcclim-render-stack::translate-sdl3-event)))
