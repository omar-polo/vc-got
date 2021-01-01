;;; vc-got.el --- Game of Tree backend for VC        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Polo

;; Author: Omar Polo <op@venera>
;; Keywords: vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
;; - dir-extra-headers                  DONE
;; - dir-printer                        NOT IMPLEMENTED
;; - status-fileinfo-extra              NOT IMPLEMENTED
;; * working-revision                   DONE
;; * checkout-model                     DONE
;; - mode-line-string                   DONE
;;
;; STATE-CHANGING FUNCTIONS:
;; * create-repo                        NOT IMPLEMENTED
;;      I don't think got init does what this function is supposed to
;;      do.
;; * register                           DONE
;; - responsible-p                      DONE
;; - receive-file                       NOT IMPLEMENTED
;; - unregister                         NOT IMPLEMENTED
;;      use remove?
;; * checkin                            DONE
;; * find-revision                      DONE
;; * checkout                           NOT IMPLEMENTED
;;      I'm not sure how to properly implement this.  Does filling
;;      FILE with the find-revision do the trick?  Or use got update?
;; * revert                             DONE
;; - merge-file                         NOT IMPLEMENTED
;; - merge-branch                       DONE
;; - merge-news                         NOT IMPLEMENTED
;; - pull                               DONE
;; - push                               DONE
;;      uses git
;; - steal-lock                         NOT IMPLEMENTED
;; - modify-change-comment              NOT IMPLEMENTED
;;      can be implemented via histedit, if I understood correctly
;;      what it is supposed to do.
;; - mark-resolved                      NOT IMPLEMENTED
;; - find-admin-dir                     NOT IMPLEMENTED
;;
;; HISTORY FUNCTIONS
;; * print-log                          DONE
;; * log-outgoing                       DONE
;; * log-incoming                       DONE
;; - log-search                         DONE
;; - log-view-mode                      NOT IMPLEMENTED
;; - show-log-entry                     NOT IMPLEMENTED
;; - comment-history                    NOT IMPLEMENTED
;; - update-changelog                   NOT IMPLEMENTED
;; * diff                               DONE
;; - revision-completion-table          NOT IMPLEMENTED
;; - annotate-command                   DONE
;; - annotate-time                      DONE
;; - annotate-current-time              NOT IMPLEMENTED
;; - annotate-extract-revision-at-line  DONE
;; - region-history                     NOT IMPLEMENTED
;; - region-history-mode                NOT IMPLEMENTED
;; - mergebase                          NOT IMPLEMENTED
;;
;; TAG SYSTEM
;; - create-tag                         NOT IMPLEMENTED
;; - retrieve-tag                       NOT IMPLEMENTED
;;
;; MISCELLANEOUS                        NOT IMPLEMENTED
;; - make-version-backups-p             NOT IMPLEMENTED
;; - root                               DONE
;; - ignore                             NOT IMPLEMENTED
;; - ignore-completion-table            NOT IMPLEMENTED
;; - previous-revision                  DONE
;; - next-revision                      DONE
;; - log-edit-mode                      NOT IMPLEMENTED
;; - check-headers                      NOT IMPLEMENTED
;; - delete-file                        NOT IMPLEMENTED
;; - rename-file                        NOT IMPLEMENTED
;; - find-file-hook                     NOT IMPLEMENTED
;; - extra-menu                         NOT IMPLEMENTED
;; - extra-dir-menu                     NOT IMPLEMENTED
;; - conflicted-files                   NOT IMPLEMENTED
;; - repository-url                     NOT IMPLEMENTED

;; TODO: use the idiom
;;      (let (process-file-side-effects) ...)
;; when the got command WON'T change the file.  This can enable some
;; emacs optimizations

;; TODO: vc-git has most function that starts with:
;;
;;    (let* ((root (vc-git-root default-directory))
;;           (buffer (format "*vc-git : %s*" (expand-file-name root)))
;;           ...)
;;      ...)
;;
;; we should 1) investigate if also other backends do something like
;; this (or if there is a better way) and 2) try to do the same.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'cl-lib)
(require 'cl-seq)
(require 'seq)
(require 'vc)

(require 'vc-got-stage)

(defgroup vc-got nil
  "VC GoT backend."
  :group 'vc)

(defcustom vc-got-program "got"
  "Name of the Got executable (excluding any arguments)."
  :type 'string
  :group 'vc-got)

(defcustom vc-got-diff-switches t
  "String or list of strings specifying switches for Got diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-got)

;; helpers
(defun vc-got--program-version ()
  "Returns the version string of used `Got' command."
  (let (process-file-side-effects)
    (with-temp-buffer
      (vc-got--call "-V")
      (substring (buffer-string) 4 -1))))

(defun vc-got-root (file)
  "Return the work tree root for FILE, or nil."
  (or (vc-file-getprop file 'got-root)
      (vc-file-setprop file 'got-root (vc-find-root file ".got"))))

(defmacro vc-got-with-worktree (file &rest body)
  "Evaluate BODY in the work tree directory of FILE."
  (declare (indent defun))
  `(when-let (default-directory (vc-got-root ,file))
     ,@body))

(defun vc-got--repo-root ()
  "Return the path to the repository root.
Assume `default-directory' is inside a got worktree."
  (vc-got-with-worktree default-directory
    (with-temp-buffer
      (insert-file-contents ".got/repository")
      (string-trim (buffer-string) nil "\n"))))

(defun vc-got--call (&rest args)
  "Call `vc-got-program' in the `default-directory' with ARGS and put the output in the current buffer."
  (apply #'process-file vc-got-program nil (current-buffer) nil args))

(defun vc-got--add (files)
  "Add FILES to got, passing `vc-register-switches' to the command invocation."
  (with-temp-buffer
    (apply #'vc-got--call "add" (append vc-register-switches files))))

(defun vc-got--log (&optional path limit start-commit stop-commit
                              search-pattern reverse)
  "Execute the log command in the worktree of PATH.
The output in the current buffer.

LIMIT limits the maximum number of commit returned.

START-COMMIT: start traversing history at the specified commit.
STOP-COMMIT: stop traversing history at the specified commit.
SEARCH-PATTERN: limit to log messages matched by the regexp given.
REVERSE: display the log messages in reverse order.

Return nil if the command failed or if PATH isn't included in any
worktree."
  (let (process-file-side-effects)
    (vc-got-with-worktree (or path default-directory)
      (zerop
       (apply #'vc-got--call
              (cl-remove-if #'null
                            (flatten-list
                             (list "log"
                                   (when limit (list "-l" (format "%s" limit)))
                                   (when start-commit (list "-c" start-commit))
                                   (when stop-commit (list "-x" stop-commit))
                                   (when search-pattern (list "-s" search-pattern))
                                   (when reverse '("-R"))
                                   path))))))))

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

(defun vc-got--revert (&rest files)
  "Execute got revert FILES..."
  (vc-got-with-worktree (car files)
    (with-temp-buffer
      (apply #'vc-got--call "revert" files))))

(defun vc-got--list-branches ()
  "Return an alist of (branch . commit)."
  (with-temp-buffer
    (when (zerop (vc-got--call "branch" "-l"))
      (goto-char (point-min))
      (cl-loop
       until (= (point) (point-max))
       ;; parse the `* $branchname: $commit', from the end
       collect (let* ((_ (move-end-of-line nil))
                      (end-commit (point))
                      (_ (backward-word))
                      (start-commit (point))
                      (_ (backward-char 2))
                      (end-branchname (point))
                      (_ (move-beginning-of-line nil))
                      (_ (forward-char 2))
                      (start-branchname (point))
                      (branchname (buffer-substring start-branchname end-branchname))
                      (commit (buffer-substring start-commit end-commit)))
                 (forward-line)
                 (move-beginning-of-line nil)
                 `(,branchname . ,commit))))))

(defun vc-got--current-branch ()
  "Return the current branch."
  (with-temp-buffer
    (when (zerop (vc-got--call "branch"))
      (string-trim (buffer-string) "" "\n"))))

(defun vc-got--integrate (branch)
  "Integrate BRANCH into the current one."
  (with-temp-buffer
    (vc-got--call "integrate" branch)))

(defun vc-got--diff (&rest args)
  "Call got diff with ARGS.  The result will be stored in the current buffer."
  (apply #'vc-got--call "diff"
         (append (vc-switches 'got 'diff)
                 (mapcar #'file-relative-name args))))


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
    (when (vc-find-root file ".got")
      (let ((status (vc-got--status file)))
        (not (or (string-prefix-p "?" status)
                 (string-prefix-p "N" status)))))))

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

(defun vc-got-dir-extra-headers (_dir)
  (concat
   (propertize "Branch     : " 'face 'font-lock-type-face)
   (vc-got--current-branch)))

(defun vc-got-working-revision (file)
  "Return the id of the last commit that touched the FILE or \"0\" for a new (but added) file."
  (or
   (with-temp-buffer
     (when (vc-got--log file 1)
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

(defun vc-got-mode-line-string (file)
  "Return the VC mode line string for FILE."
  (vc-got-with-worktree file
    (let ((def (vc-default-mode-line-string 'Got file)))
      (concat (substring def 0 4) (vc-got--current-branch)))))


;; state-changing functions

(defun vc-got-create-repo (_backend)
  (error "vc got: create-repo not implemented"))

(defun vc-got-register (files &optional _comment)
  "Register FILES, passing `vc-register-switches' to the backend command."
  (vc-got--add files))

(defalias 'vc-got-responsible-p #'vc-got-root)

(defun vc-got-checkin (files comment &optional _rev)
  "Commit FILES with COMMENT as commit message."
  (with-temp-buffer
    (apply #'vc-got--call "commit" "-m"
           ;; emacs add ``Summary:'' at the start of the commit
           ;; message.  vc-git doesn't seem to treat this specially.
           ;; Since it's annoying, remove it.
           (string-remove-prefix "Summary: " comment)
           files)))

(defun vc-got-find-revision (file rev buffer)
  "Fill BUFFER with the content of FILE in the given revision REV."
  (when-let (obj-id (assoc file (vc-got--tree rev file) #'string=))
    (with-current-buffer buffer
      (vc-got-with-worktree file
        (vc-got--cat rev obj-id)))))

(defun vc-got-find-ignore-file (file)
  "Return the gitignore file that controls FILE."
  (expand-file-name ".gitignore"
                    (vc-got-root file)))

(defun vc-got-checkout (_file &optional _rev)
  "Checkout revision REV of FILE.  If REV is t, checkout from the head."
  (error "vc got: checkout not implemented"))

(defun vc-got-revert (file &optional _content-done)
  "Revert FILE back to working revision."
  (vc-got--revert file))

(defun vc-got-merge-branch ()
  "Prompt for a branch and integrate it into the current one."
  ;; XXX: be smart and try to "got rebase" if "got integrate" fails?
  (let* ((branches (cl-loop for (branch . commit) in (vc-got--list-branches)
                            collect branch))
         (branch (completing-read "Merge from branch: " branches)))
    (when branch
      (vc-got--integrate branch))))

(defun vc-got--push-pull (cmd op prompt root)
  "Execute CMD OP, or prompt the user if PROMPT is non-nil.
ROOT is the worktree root."
  (let ((buffer (format "*vc-got : %s*" (expand-file-name root))))
    (when-let (cmd (if prompt
                       (split-string
                        (read-shell-command (format "%s %s command: " cmd op)
                                            (format "%s %s" cmd op))
                        " " t)
                     (list cmd op)))
      (apply #'vc-do-command buffer 0 (car cmd) nil (cdr cmd)))))

(defun vc-got-pull (prompt)
  "Execute got pull, prompting the user for the full command if PROMPT is not nil."
  (vc-got--push-pull vc-got-program "fetch" prompt (vc-got-root default-directory)))

(defun vc-got-push (prompt)
  "Run git push (not got!) in the repository dir.
If PROMPT is non-nil, prompt for the git command to run."
  (let* ((root (vc-got-root default-directory))
         (default-directory (vc-got--repo-root)))
    (vc-got--push-pull "git" "push" prompt root)))

(defun vc-got-print-log (files buffer &optional _shortlog start-revision limit)
  "Insert the revision log for FILES into BUFFER.

LIMIT limits the number of commits, optionally starting at START-REVISION."
  (with-current-buffer buffer
    ;; the *vc-diff* may be read only
    (let ((inhibit-read-only t))
      (cl-loop for file in files
               do (vc-got--log (file-relative-name file) limit start-revision)))))

;; XXX: this includes also the latest commit in REMOTE-LOCATION.
(defun vc-got-log-outgoing (buffer remote-location)
  "Fill BUFFER with the diff between the local worktree branch and REMOTE-LOCATION."
  (vc-setup-buffer buffer)
  (let ((rl (if (or (not remote-location) (string-empty-p remote-location))
                (concat "origin/" (vc-got--current-branch))
              remote-location))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (vc-got--log nil nil nil rl))))

(defun vc-got-incoming (buffer remote-location)
  "Fill BUFFER with the diff between the REMOTE-LOCATION and the local worktree branch."
  (let ((rl (if (or (not remote-location) (string-empty-p remote-location))
                (concat "origin/" (vc-got--current-branch))
              remote-location))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (vc-got--log nil nil (vc-got--current-branch) rl))))

(defun vc-got-log-search (buffer pattern)
  "Search commits for PATTERN and write the results found in BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (vc-got--log nil nil nil nil pattern))))

;; TODO: async
;; TODO: return 0 or 1
(defun vc-got-diff (files &optional rev1 rev2 buffer _async)
  "Insert into BUFFER (or *vc-diff*) the diff for FILES from REV1 to REV2."
  (message "vc-got: debug: files is %s" files)
  (let* ((buffer (get-buffer-create (or buffer "*vc-diff*")))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (vc-got-stage-mode +1)
      ;; TODO: this shouldn't be done in an unconditioned fashion.  If
      ;; we're diffing two revision, we can't stage hunks; we can
      ;; stage only when diffing the local modifications.
      (setq vc-got-stage-fileset files)
      (vc-got-with-worktree (car files)
        (cond ((and (null rev1)
                    (null rev2))
               (dolist (file files)
                 (vc-got--diff file)))
              (t (error "Not implemented")))))))

(defun vc-got-annotate-command (file buf &optional rev)
  "Show annotated contents of FILE in buffer BUF. If given, use revision REV."
  (let (process-file-side-effects)
    (save-current-buffer
      (set-buffer buf)
      (apply #'vc-got--call "blame" (if rev
                                        (list "-c" rev file)
                                      (list file))))))

(defun vc-got-annotate-time ()
  "Return the time of the next line of annotation at or after point.
Value is returned as floating point fractional number of days."
  (save-excursion
    (beginning-of-line)
    (when (looking-at  "^[0-9]\\{1,\\}) \\([a-z0-9]+\\) \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) .+")
      (let ((str (match-string-no-properties 2)))
        (vc-annotate-convert-time
         (encode-time 0 0 0
                      (string-to-number (substring str 8 10))
                      (string-to-number (substring str 5 7))
                      (string-to-number (substring str 0 4))))))))

(defun vc-got-annotate-extract-revision-at-line ()
  "Returns revision corresponding to the current line or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at  "^[0-9]\\{1,\\}) \\([a-z0-9]+\\) \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) .+")
      (match-string-no-properties 1))))

(defun vc-got-previous-revision (file rev)
  "Return the revision number that precedes REV for FILE, or nil if no such revision exists."
  (let (process-file-side-effects)
    (vc-got-with-worktree file
      (with-temp-buffer
        (when (zerop (apply #'vc-got--call "log" (list "-c" rev "-l" "2" "-R" file)))
          (keep-lines "^commit")
          (when (looking-at "^commit \\([a-z0-9]+\\)")
            (match-string-no-properties 1)))))))

(defun vc-got-next-revision (file rev)
  "Return the revision number that follows REV for FILE, or nil
  if no such revision exists."
  (let (process-file-side-effects)
    (vc-got-with-worktree file
      (with-temp-buffer
        (when (zerop (apply #'vc-got--call "log" (list "-x" rev file)))
          (keep-lines "^commit")
          (goto-char (point-max))
          (beginning-of-line)
          (previous-line) ;; return from empty line to last actual commit
          (when (looking-at  "^commit \\([a-z0-9]+\\)$")
            ;; no need to continue if looking at top commit
            (unless (string= rev (match-string-no-properties 1))
              (previous-line)
              (when (looking-at  "^commit \\([a-z0-9]+\\)")
                (match-string-no-properties 1)))))))))

(provide 'vc-got)
;;; vc-got.el ends here
