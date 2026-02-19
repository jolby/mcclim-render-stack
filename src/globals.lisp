;;;; globals.lisp — Global render-stack engine and delegate for McCLIM
;;;;
;;;; This file defines the global engine and delegate that are shared
;;;; across all McCLIM ports in the application.

(in-package :mcclim-render-stack)

;;; ============================================================================
;;; Global State Variables
;;; ============================================================================

(defvar *global-engine* nil
  "The global render-engine instance. Shared by all McCLIM ports.
   Runs on the main thread. Created by initialize-global-engine.
   Thread-safe: Read-only after initialization.")

(defvar *global-delegate* nil
  "The global multi-window-render-delegate instance.
   Thread-safe: Methods use internal locking.")

(defvar *global-engine-initialized* nil
  "T if global engine has been initialized.
   Thread-safe: Set once during initialization.")

(defvar *global-impeller-context* nil
  "The global Impeller GL rendering context.
   Created on the main thread after the first SDL3 GL context is made current.
   Must be created with frs:make-context + SDL3 GL proc address callback.
   Thread-safe: Read-only after initialization.")

;;; ============================================================================
;;; Impeller Context Management
;;; ============================================================================

;;; GL proc address callback for Impeller context creation.
;;; SDL3 provides the proc getter; we register it as a CFFI callback so
;;; Impeller can resolve OpenGL ES entry points at context creation time.
(cffi:defcallback sdl3-gl-proc-getter :pointer
    ((proc-name :string) (user-data :pointer))
  (declare (ignore user-data))
  (%sdl3:gl-get-proc-address proc-name))

(defun initialize-global-impeller-context ()
  "Create the global Impeller GL rendering context.

   MUST be called on the main thread after an SDL3 GL context has been made
   current (i.e., after rs-sdl3:make-sdl3-window has run on the main thread).
   Idempotent: safe to call multiple times.

   Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread initialize-global-impeller-context)
  (unless *global-impeller-context*
    (log:info :mcclim-render-stack "Creating global Impeller GL context")
    (setf *global-impeller-context*
          (rs-internals:without-float-traps
            (frs:make-context :gl-proc-address-callback
                              (cffi:callback sdl3-gl-proc-getter))))
    (log:info :mcclim-render-stack "Global Impeller GL context created: ~A"
              *global-impeller-context*)))

;;; ============================================================================
;;; Global Engine Management
;;; ============================================================================

(defun initialize-global-engine ()
  "Initialize the global render-engine and delegate.
   Must be called on the main thread before any ports are created.
   Idempotent: safe to call multiple times.
   
   Thread Contract: MUST be called on main thread."
  (rs-internals:assert-main-thread initialize-global-engine)
  
  (unless *global-engine-initialized*
    (log:info :mcclim-render-stack "Initializing global render engine")
    
    ;; Create delegate
    (setf *global-delegate* (make-instance 'multi-window-render-delegate))
    
    ;; Create engine
    (setf *global-engine* (render-stack:make-render-engine
                           :delegate *global-delegate*
                           :pipeline-depth 2
                           :target-fps 60))
    
    ;; Start engine
    (render-stack:render-engine-start *global-engine*)
    
    (setf *global-engine-initialized* t)
    (log:info :mcclim-render-stack "Global render engine initialized")))

(defun shutdown-global-engine ()
  "Shutdown the global render-engine.
   Should be called on application exit.
   
   Thread Contract: SHOULD be called on main thread."
  (when *global-engine-initialized*
    (log:info :mcclim-render-stack "Shutting down global render engine")
    
    (when *global-engine*
      (render-stack:render-engine-stop *global-engine*)
      (setf *global-engine* nil))

    (when *global-impeller-context*
      (rs-internals:without-float-traps
        (frs:release-context *global-impeller-context*))
      (setf *global-impeller-context* nil))

    (setf *global-delegate* nil)
    (setf *global-engine-initialized* nil)

    (log:info :mcclim-render-stack "Global render engine shutdown complete")))

;;; ============================================================================
;;; Cleanup
;;; ============================================================================

(defun reset-global-state ()
  "Reset all global state variables. Useful for testing.
   
   Thread Contract: SHOULD be called on main thread."
  (setf *global-engine* nil
        *global-delegate* nil
        *global-engine-initialized* nil
        *global-impeller-context* nil))
