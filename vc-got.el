;; vc-got.el --- Game of Tree backend for VC -*- lexical-binding: t; -*-

;; Copyright © 2020 Omar Polo <op@omarpolo.com>

;; This file is not part of GNU Emacs.

;; This file is free software.
;;
;; Permission to use, copy, modify, and distribute this software for
;; any purpose with or without fee is hereby granted, provided that
;; the above copyright notice and this permission notice appear in all
;; copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;; Author: Omar Polo <op@omarpolo.com>
;; URL: https://git.omarpolo.com/vc-got
;; Keywords: vc vc-backend

;;; Commentary

;; Backend implementation status
;;
;; Function marked with `*' are required, those with `-' are optional.
;;
;; FUNCTION NAME                        STATUS
;;
;; BACKEND PROPERTIES:
;; * revision-granularity               DONE
;; - update-on-retrieve-tag             XXX: what should this do?
;;
;; STATE-QUERYING FUNCTIONS:
;; * registered                         DONE
;; * state                              DONE
;; - dir-status-files                   DONE
;; - dir-extra-headers                  NOT IMPLEMENTED
;; - dir-printer                        NOT IMPLEMENTED
;; - status-fileinfo-extra              NOT IMPLEMENTED
;; * working-revision                   DONE
;; * checkout-model                     DONE
;; - mode-line-string                   NOT IMPLEMENTED
;;
;; STATE-CHANGING FUNCTIONS:
;; * create-repo                        NOT IMPLEMENTED
;;                           I don't think got init does
;;                           what this function is supposed
;;                           to do.
;; * register                           DONE
;; - responsible-p                      DONE
;; - receive-file                       NOT IMPLEMENTED
;; - unregister                         NOT IMPLEMENTED
;;                           use remove?
;; * checkin                            DONE
;; * find-revision                      DONE

;; TODO: use the idiom
;;      (let (process-file-side-effects) ...)
;; when the got command WON'T change the file.  This can enable some
;; emacs optimizations

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'cl-lib)
(require 'seq)
(require 'vc)

(defvar vc-got-cmd "got"
  "The got command.")

;; helpers

(defun vc-got-root (file)
  "Return the work tree root for FILE, or nil."
  (or (vc-file-getprop file 'git-root)
      (vc-file-setprop file 'git-root (vc-find-root file ".got"))))

(defmacro vc-got-with-worktree (file &rest body)
  "Evaluate BODY in the work tree directory of FILE."
  (declare (indent defun))
  `(when-let (default-directory (vc-got-root ,file))
     ,@body))

(defun vc-got--call (&rest args)
  "Call `vc-got-cmd' in the `default-directory' with ARGS and put the output in the current buffer."
  (apply #'process-file vc-got-cmd nil (current-buffer) nil args))

(defun vc-got--add (files)
  "Add FILES to got, passing `vc-register-switches' to the command invocation."
  (with-temp-buffer
    (apply #'vc-got--call "add" (append vc-register-switches files))))

(defun vc-got--log (limit path)
  "Execute the log command in the worktree of PATH.

The output of the command will be put in the current-buffer.

LIMIT limits the maximum number of commit returned.

Return nil if the command failed or if PATH isn't included in any
worktree."
  (vc-got-with-worktree path
    (zerop (vc-got--call "log" "-l" (format "%s" limit) path))))

(defun vc-got--status (dir-or-file &rest files)
  "Return the output of ``got status''.

DIR-OR-FILE can be either a directory or a file.  If FILES is
given, return the status of those files, otherwise the status of
DIR-OR-FILE."
  (vc-got-with-worktree dir-or-file
    (with-temp-buffer
      (if files
          (apply #'vc-got--call "status" files)
        (vc-got--call "status" dir-or-file))
      (buffer-string))))

(defun vc-got--parse-status-flag (flag)
  "Parse FLAG, see `vc-state'."
  ;; got outputs nothing if the file is up-to-date
  (if (string-empty-p flag)
      'up-to-date
    ;; trying to follow the order of the manpage
    (cl-case (aref flag 0)
      (?M 'edited)
      (?A 'added)
      (?D 'removed)
      (?C 'conflict)
      (?! 'missing)
      (?~ 'edited) ;XXX: what does it means for a file to be ``obstructed''?
      (?? 'unregistered)
      (?m 'edited) ;modified file modes
      (?N nil))))

(defun vc-got--parse-status (output)
  "Parse the OUTPUT of got status and return an alist of (FILE . STATUS)."
  ;; XXX: the output of got is line-oriented and will break if
  ;; filenames contains spaces or newlines.
  (cl-loop for line in (split-string output "\n" t)
           collect (cl-destructuring-bind (status file) (split-string line " " t " ")
                     `(,file . ,(vc-got--parse-status-flag status)))))

(defun vc-got--tree-parse ()
  "Parse into an alist the output of got tree -i in the current buffer."
  (goto-char (point-min))
  (cl-loop
   until (= (point) (point-max))
   collect (let* ((obj-start (point))
                  (_ (forward-word))
                  (obj (buffer-substring obj-start (point)))
                  (_ (forward-char))         ;skip the space
                  (filename-start (point))
                  (_ (move-end-of-line nil))
                  (filename (buffer-substring filename-start (point))))
             ;; goto the start of the next line
             (forward-line)
             (move-beginning-of-line nil)
             `(,filename . ,obj))))

(defun vc-got--tree (commit path)
  (vc-got-with-worktree path
    (with-temp-buffer
      (vc-got--call "tree" "-c" commit "-i" path)
      (vc-got--tree-parse))))

(defun vc-got--cat (commit obj-id)
  "Execute got cat -c COMMIT OBJ-ID in the current buffer."
  (vc-got--call "cat" "-c" commit obj-id))


;; Backend properties

(defun vc-got-revision-granularity ()
  "Got has REPOSITORY granularity."
  'repository)

;; XXX: what this should do?  The description is not entirely clear
(defun vc-got-update-on-retrieve-tag ()
  nil)


;; State-querying functions

;;;###autoload (defun vc-got-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with got."
;;;###autoload   (when (vc-find-root file ".got")
;;;###autoload     (load "vc-got" nil t)
;;;###autoload     (vc-got-registered file)))

(defun vc-got-registered (file)
  "Return non-nil if FILE is registered with got."
  (if (file-directory-p file)
      nil                               ;got doesn't track directories
    (let ((status (vc-got--status file)))
      (not (or (string-prefix-p "?" status)
               (string-prefix-p "N" status))))))

;; (vc-got-registered "/usr/ports/mystuff/net/td")
;; (vc-got-registered "/usr/ports/mystuff/net/td/Makefile")
;; (vc-got-registered "/usr/ports/mystuff/tmp")
;; (vc-got-registered "/usr/ports/mystuff/no-existant")

(defun vc-got-state (file)
  "Return the current version control state of FILE.  See `vc-state'."
  (unless (file-directory-p file)
    (vc-got--parse-status-flag (vc-got--status file))))

;; (vc-got-state "/usr/ports/mystuff/net/td")
;; (vc-got-state "/usr/ports/mystuff/net/td/Makefile")
;; (vc-got-state "/usr/ports/mystuff/tmp")
;; (vc-got-state "/usr/ports/mystuff/non-existant")

(defun vc-got-dir-status-files (dir files update-function)
  (let* ((files (seq-filter (lambda (file)
                              (and (not (string= file ".."))
                                   (not (string= file "."))
                                   (not (string= file ".got"))))
                            (or files
                                (directory-files dir))))
         (statuses (vc-got--parse-status
                    (apply #'vc-got--status dir files)))
         (default-directory dir))
    (cl-loop
     with result = nil
     for file in files
     do (setq result
              (cons
               (if (file-directory-p file)
                   (list file 'unregistered nil)
                 (if-let (status (cdr (assoc file statuses #'string=)))
                     (list file status nil)
                   (list file 'up-to-date nil)))
               result))
     finally (funcall update-function result nil))))

;; (let ((dir "/usr/ports/mystuff"))
;;   (vc-got-dir-status-files dir nil (lambda (res _t)
;;                                      (message "got %s" res))))

(defun vc-got-working-revision (file)
  "Return the id of the last commit that touched the FILE.

Return \"0\" for a file added but not yet committed."
  (or
   (with-temp-buffer
     (when (vc-got--log 1 file)
       (let (start)
         (goto-char (point-min))
         (forward-line 1)               ;skip the ----- line
         (forward-word)                 ;skip "commit"
         (forward-char)                 ;skip the space
         (setq start (point))           ;store start of the SHA
         (forward-word)                 ;goto SHA end
         (buffer-substring start (point)))))
   ;; special case: if this file is added but has no previous commits
   ;; touching it, got log will fail (as expected), but we have to
   ;; return "0".
   (when (eq (vc-got-state file) 'added)
     "0")))

;; (vc-got-working-revision "/usr/ports/mystuff/non-existant")
;; (vc-got-working-revision "/usr/ports/mystuff/CVS")
;; (vc-got-working-revision "/usr/ports/mystuff/tmp")
;; (vc-got-working-revision "/usr/ports/mystuff/net/td/Makefile")

(defun vc-got-checkout-model (_files)
  'implicit)


;; state-changing functions

(defun vc-got-create-repo (_backend)
  (error "vc got: create-repo not implemented"))

(defun vc-got-register (files &optional _comment)
  "Register FILES, passing `vc-register-switches' to the backend command."
  (vc-got--add files))

(defun vc-got-responsible-p (file)
  (vc-find-root file ".got"))

(defun vc-got-checkin (files comment &optional _rev)
  "Commit FILES with COMMENT as commit message."
  (with-temp-buffer
    (apply #'vc-got--call "commit" "-m" comment files)))

(defun vc-got-find-revision (file rev buffer)
  (when-let (obj-id (assoc file (vc-got--tree rev file) #'string=))
    (with-current-buffer buffer
      (vc-got-with-worktree file
        (vc-got--cat rev obj-id)))))

(provide 'vc-got)
;;; vc-got.el ends here
