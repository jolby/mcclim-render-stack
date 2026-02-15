;;;; mcclim-render-stack/test/test-utils.lisp
;;;; Native library availability probes and test skip macros

(in-package :mcclim-render-stack-tests)

;;; Native library availability probes
;;; These are checked once at first use; integration tests skip honestly
;;; (showing as SKIPPED, not falsely PASSED) when native libs are absent.

(defvar *sdl3-native-available* :untested)

(defun sdl3-native-available-p ()
  "Probe whether the SDL3 native library is loadable.
Caches the result after the first call."
  (when (eq *sdl3-native-available* :untested)
    (setf *sdl3-native-available*
          (handler-case
              (progn
                ;; Try to call a simple SDL3 function
                (render-stack-sdl3::init-sdl3-video)
                (render-stack-sdl3::quit-sdl3)
                t)
            (error () nil))))
  *sdl3-native-available*)

(defmacro skip-unless-sdl3 (&body body)
  "Execute BODY if SDL3 native library is available, otherwise skip.
Skipped tests show as SKIPPED in Parachute output rather than falsely passing."
  `(if (sdl3-native-available-p)
       (progn ,@body)
       (skip "SDL3 native library not available"
         (true t))))
