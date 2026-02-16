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
        *global-engine-initialized* nil))
