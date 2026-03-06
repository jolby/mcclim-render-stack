;;;; server.lisp -- File-polling eval server for Claude Code skill integration
;;;;
;;;; Starts a background thread that polls a /tmp directory for command files,
;;;; evaluates each Lisp expression in the running image, and writes the result
;;;; back to a result file.  Shell scripts and Claude Code skills drive this
;;;; server to capture frames, inject events, and read render output.
;;;;
;;;; WARNING: This is a development-only tool.  It evaluates arbitrary Lisp
;;;; forms in the running image.  Never enable in production or expose to
;;;; untrusted input.
;;;;
;;;; Protocol (all files under SERVER-DIR, default /tmp/rs-server/):
;;;;   cmd.sexp    -- shell writes Lisp expression here
;;;;   cmd.ready   -- shell touches this file to signal command is ready
;;;;   result.sexp -- server writes printed result here
;;;;   result.done -- server touches this file to signal result is available
;;;;
;;;; Shell client:
;;;;   scripts/rs-eval.sh 'EXPR'   -- evaluate EXPR, print result, exit

(in-package :mcclim-render-stack/test-rig)

(defvar *repl-server-thread* nil
  "The background thread running the eval server, or NIL if stopped.")

(defvar *repl-server-running* nil
  "T while the eval server loop should keep running.")

(defvar *repl-server-dir* "/tmp/rs-server"
  "Directory used for command/result files.  Matches scripts/rs-eval.sh default.")

;;; ============================================================================
;;; Server loop
;;; ============================================================================

(defun %repl-server-loop (dir)
  (let ((cmd-file    (uiop:merge-pathnames* "cmd.sexp"    (uiop:ensure-directory-pathname dir)))
        (ready-file  (uiop:merge-pathnames* "cmd.ready"   (uiop:ensure-directory-pathname dir)))
        (result-file (uiop:merge-pathnames* "result.sexp" (uiop:ensure-directory-pathname dir)))
        (done-file   (uiop:merge-pathnames* "result.done" (uiop:ensure-directory-pathname dir))))
    (log:info :test-rig "Eval server polling ~A" dir)
    (loop while *repl-server-running* do
      (when (probe-file ready-file)
        ;; Clear ready flag first so a slow eval doesn't re-trigger.
        (ignore-errors (delete-file ready-file))
        (let ((result
                (handler-case
                    (with-open-file (in cmd-file :direction :input
                                                 :if-does-not-exist nil)
                      (if in
                          (let ((expr (read in nil :eof)))
                            (if (eq expr :eof)
                                "NIL ; empty command"
                                (with-output-to-string (s)
                                  (write (eval expr) :stream s))))
                          "ERROR: cmd.sexp not found"))
                  (error (e)
                    (format nil "ERROR: ~A" e)))))
          (ignore-errors (delete-file done-file))
          (with-open-file (out result-file :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
            (write-string result out)
            (terpri out))
          (with-open-file (done done-file :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
            (write-string "done" done))
          (log:debug :test-rig "Eval server result: ~A" result)))
      (sleep 0.05))))

;;; ============================================================================
;;; Public API
;;; ============================================================================

(defun start-repl-server (&optional (dir *repl-server-dir*))
  "Start the file-polling eval server in a background thread.
Polls DIR (default /tmp/rs-server/) for command files and evaluates them.
Safe to call from REPL.  Returns the server directory path."
  (when *repl-server-running*
    (log:warn :test-rig "Eval server already running in ~A; stop first." *repl-server-dir*)
    (return-from start-repl-server *repl-server-dir*))
  (ensure-directories-exist (uiop:ensure-directory-pathname dir))
  (setf *repl-server-dir* dir
        *repl-server-running* t
        *repl-server-thread*
        (bt2:make-thread
         (lambda () (%repl-server-loop dir))
         :name "rs-repl-server"))
  (log:info :test-rig "Eval server started. Shell client: scripts/rs-eval.sh 'EXPR'")
  dir)

(defun stop-repl-server ()
  "Stop the eval server background thread.  Safe to call from any thread."
  (setf *repl-server-running* nil)
  (when *repl-server-thread*
    (bt2:join-thread *repl-server-thread* :timeout 2)
    (setf *repl-server-thread* nil))
  (log:info :test-rig "Eval server stopped.")
  (values))
