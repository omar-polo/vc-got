;;; vc-got.el --- VC backend for Game of Trees VCS   -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; Author: Omar Polo <op@omarpolo.com>
;;         Timo Myyrä <timo.myyra@bittivirhe.fi>
;; URL: https://projects.omarpolo.com/vc-got.html
;; Keywords: vc tools
;; Version: 1.2
;; Package-Requires: ((emacs "25.1"))

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

;;; Commentary:

;; vc-got is a VC backend for the Game of Trees (got) version control
;; system.
;;
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
;; - dir-printer                        DONE
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
;; - receive-file                       NOT NEEDED, default `register' is fine
;; - unregister                         DONE
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
;; - steal-lock                         NOT NEEDED, `got' is not using locks
;; - modify-change-comment              NOT IMPLEMENTED
;;      can be implemented via histedit, if I understood correctly
;;      what it is supposed to do.
;; - mark-resolved                      NOT NEEDED
;;      got notice by itself when a file doesn't have any pending
;;      conflicts to be resolved.
;; - find-admin-dir                     NOT NEEDED
;;
;; HISTORY FUNCTIONS
;; * print-log                          DONE
;; * log-outgoing                       DONE
;; * log-incoming                       DONE
;; - log-search                         DONE
;; - log-view-mode                      DONE
;; - show-log-entry                     NOT IMPLEMENTED
;; - comment-history                    NOT IMPLEMENTED
;; - update-changelog                   NOT IMPLEMENTED
;; * diff                               DONE
;; - revision-completion-table          DONE
;; - annotate-command                   DONE
;; - annotate-time                      DONE
;; - annotate-current-time              NOT NEEDED
;;      the default time handling is enough.
;; - annotate-extract-revision-at-line  DONE
;; - region-history                     NOT IMPLEMENTED
;; - region-history-mode                NOT IMPLEMENTED
;; - mergebase                          NOT IMPLEMENTED
;;
;; TAG SYSTEM
;; - create-tag                         DONE
;; - retrieve-tag                       DONE
;;
;; MISCELLANEOUS                        NOT IMPLEMENTED
;; - make-version-backups-p             NOT NEEDED, `got' works fine locally
;; - root                               DONE
;; - ignore                             NOT NEEDED, the default action is good
;; - ignore-completion-table            NOT NEEDED, the default action is good
;; - find-ignore-file                   DONE
;; - previous-revision                  DONE
;; - next-revision                      DONE
;; - log-edit-mode                      NOT IMPLEMENTED
;; - check-headers                      NOT NEEDED, `got' does not use headers
;; - delete-file                        DONE
;; - rename-file                        NOT IMPLEMENTED
;; - find-file-hook                     DONE
;; - extra-menu                         NOT IMPLEMENTED
;; - extra-dir-menu                     NOT IMPLEMENTED, same as above
;; - conflicted-files                   DONE
;; - repository-url                     DONE

;;; Code:

;; TODO: vc-git has most function that starts with:
;;
;;    (let* ((root (vc-git-root default-directory))
;;           (buffer (format "*vc-git : %s*" (expand-file-name root)))
;;           ...)
;;      ...)
;;
;; we should 1) investigate if also other backends do something like
;; this (or if there is a better way) and 2) try to do the same.

(eval-when-compile
  (require 'subr-x))

(require 'cl-lib)
(require 'seq)
(require 'vc)
(require 'vc-annotate)

;; FIXME: avoid loading this?  We only need it for
;; log-edit-extract-headers in vc-got-checkin.
(require 'log-edit)

;; FIXME: avoid loading this?  We only need it for
;; vc-dir-filename-mouse-map in our custom printer.
(require 'vc-dir)

;; FIXME: avoid loading this?  We only need it for
;; compilation-{directory,arguments}.
(require 'compile)

;; FIXME: avoid loading this?  We only need it for
;; log-view-{file-re,per-file-logs,message-re}.
(require 'log-view)

(defgroup vc-got nil
  "VC GoT backend."
  :group 'vc)

(defcustom vc-got-program "got"
  "Name of the Got executable (excluding any arguments)."
  :type 'string)

(defcustom vc-got-diff-switches t
  "String or list of strings specifying switches for Got diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string)))

;; helpers
(defmacro vc-got--with-emacs-version<= (version &rest body)
  "Eval BODY only when the Emacs version in greater or equal VERSION."
  (declare (debug body)
           (indent defun))
  (when (version<= version emacs-version)
    `(progn ,@body)))

(defun vc-got--version ()
  "Return string representing the got version."
  (let (process-file-side-effects)
    (with-temp-buffer
      (vc-got--call "-V")
      (substring (buffer-string) 4 -1))))

(defun vc-got--version<= (target)
  "Compare the current version against TARGET.
Takes care of handling the -current suffix."
  (let* ((version-string (vc-got--version))
         (current-version (string-replace "-current" "" version-string)))
    (when (version<= current-version target)
      ;; let X.Y-current sort *after* X.Y
      (string= version-string current-version))))

(defun vc-got-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-got.el.
The difference to `vc-do-command' is that this function always invokes
`vc-got-program'."
  (let ((process-environment process-environment))
    (apply #'vc-do-command (or buffer "*vc*") okstatus vc-got-program
           file-or-list
           flags)))

(defun vc-got-root (file)
  "Return the work tree root for FILE, or nil."
  (vc-find-root file ".got"))

(defmacro vc-got-with-worktree (file &rest body)
  "Evaluate BODY in the work tree directory of FILE."
  (declare (debug t) (indent defun))
  `(when-let (default-directory (vc-got-root ,file))
     ,@body))

(defun vc-got--repo-root ()
  "Return the path to the repository root.
Assume `default-directory' is inside a got worktree."
  (vc-got-with-worktree default-directory
    (with-temp-buffer
      (insert-file-contents ".got/repository")
      (string-trim (buffer-string) "" "\n"))))

(defun vc-got--call (&rest args)
  "Call `vc-got-program' with ARGS.
The output will be placed in the current buffer."
  (apply #'process-file vc-got-program nil (current-buffer) nil
         (apply #'nconc (mapcar (lambda (s) (if (listp s) s (list s))) args))))

(defun vc-got--add (files)
  "Add FILES to got, passing `vc-register-switches' to the command invocation."
  (with-temp-buffer
    (vc-got-command t 0 files "add" vc-register-switches)))

(defun vc-got--info (path)
  "Execute got info in the worktree of PATH in the current buffer."
  (let (process-file-side-effects)
    (vc-got-with-worktree path
      (vc-got-command t 0 path "info"))))

(defun vc-got--log (&optional path limit start-commit stop-commit
                              search-pattern reverse include-diff)
  "Execute the log command in the worktree of PATH in the current buffer.
LIMIT limits the maximum number of commit returned.

START-COMMIT: start traversing history at the specified commit.
STOP-COMMIT: stop traversing history at the specified commit.
SEARCH-PATTERN: limit to log messages matched by the regexp given.
REVERSE: display the log messages in reverse order.
INCLUDE-DIFF: display the patch of modifications made in each commit.

Return nil if the command failed or if PATH isn't included in any
worktree."
  (let* ((process-file-side-effects nil)
         ;; got 0.71-current at some point switched to -S for search
         ;; pattern and -s for the one-line format.
         ;; XXX: remove in a few releases.
         (search-flag (if (vc-got--version<= "0.71")
                          "-s"
                        "-S")))
    (vc-got-with-worktree (or path default-directory)
      (save-excursion
        (apply #'vc-got-command t 0 path "log"
               (mapcan (lambda (x)
                         (if (listp x) x nil))
                       (list (and limit (list "-l" (format "%s" limit)))
                             (and start-commit (list "-c" start-commit))
                             (and stop-commit (list "-x" stop-commit))
                             (and search-pattern (list search-flag
                                                       search-pattern))
                             (and reverse '("-R"))
                             (and include-diff '("-p")))))
          t))))


(defun vc-got--status (status-codes dir-or-file &optional files)
  "Return a list of lists (FILE STATUS STAGE-STATUS).
DIR-OR-FILE can be either a directory or a file.  If FILES is
given, return the status of those files, otherwise the status of
DIR-OR-FILE.  STATUS-CODES is either nil, or a string that's
passed as the -s flag to got status to limit the types of status
to report (e.g. \"CD\" to report only conflicts and deleted
files)."
  (with-temp-buffer
    (let* ((default-directory (expand-file-name
                               (if (file-directory-p dir-or-file)
                                   dir-or-file
                                 (file-name-directory dir-or-file))))
           (root (vc-got-root default-directory))
           (process-file-side-effects))
      (apply #'vc-got-command t 0 (or files dir-or-file)
             "status" (and status-codes (list "-s" status-codes)))
      (goto-char (point-min))
      (cl-loop until (eobp)
               collect (vc-got--parse-status-line root)
               do (forward-line)))))

(defun vc-got--parse-status-line (root)
  "Parse a line of the the output of status.
ROOT is the root of the repo."
  ;; the format of each line is
  ;; <status-char> <stage-char> <spc> <filename> \n
  (let* ((file-status (prog1 (vc-got--parse-status-char
                              (char-after))
                        (forward-char)))
         (stage-status (let* ((c (char-after)))
                         (prog1
                             (when (member c '(?M ?A ?D))
                               c)
                           (forward-char))))
         (filename (progn
                     (forward-char)
                     (buffer-substring (point)
                                       (line-end-position)))))
    (list (file-relative-name (expand-file-name filename root)
                              default-directory)
          (or file-status (and stage-status 'up-to-date))
          stage-status)))

(defun vc-got--parse-status-char (c)
  "Parse status char C into a symbol accepted by `vc-state'."
  (cl-case c
    (?M 'edited)
    (?A 'added)
    (?D 'removed)
    (?C 'conflict)
    (?! 'missing)
    (?~ 'edited) ; file was replaced by e.g. a directory
    (?? 'unregistered)
    (?m 'edited) ; modified file modes
    (?N nil)))

(defun vc-got--cat (commit obj-id)
  "Execute got cat -c COMMIT OBJ-ID in the current buffer."
  (let (process-file-side-effects)
    (vc-got-command t 0 nil "cat" "-c" commit obj-id)))

(defun vc-got--revert (&rest files)
  "Execute got revert FILES."
  (vc-got-with-worktree (car files)
    (with-temp-buffer
      (vc-got-command t 0 files "revert"))))

(defun vc-got--list-branches ()
  "Return an alist of (branch . commit)."
  (let (process-file-side-effects)
    (with-temp-buffer
      (vc-got-command t 0 nil "branch" "-l")
      (let (alist)
        (goto-char (point-min))
        (while (re-search-forward "^\\*?[[:space:]]+\\(.+\\): \\([[:word:]]+\\)$"
                                  nil t)
          (push (cons (match-string 1) (match-string 2)) alist))
        alist))))

(defun vc-got--current-branch ()
  "Return the current branch."
  (let (process-file-side-effects)
    (with-temp-buffer
      (when (zerop (vc-got--call "branch"))
        (string-trim (buffer-string) "" "\n")))))

(defun vc-got--integrate (branch)
  "Integrate BRANCH into the current one."
  (with-temp-buffer
    (vc-got-command t 0 nil "integrate" branch)))

(defun vc-got--update (branch &optional paths)
  "Update to a different commit or BRANCH.
Optionally restrict the update operation to files at or within
the specified PATHS."
  (with-temp-buffer
    (unless (zerop (vc-got--call "update" "-b" branch "--" paths))
      (error "[vc-got] can't update to branch %s: %s"
             branch
             (buffer-string)))))

(defun vc-got--diff-files (files)
  "Compute the local modifications to FILES."
  (let (process-file-side-effects)
    (vc-got-command t 0 files "diff" (vc-switches 'got 'diff) "-P")))

(defun vc-got--diff-objects (obj1 obj2)
  "Diff the two objects OBJ1 and OBJ2.
OBJ1 and OBJ2 are interpreted as a reference, tag name, or an
object ID SHA1 hash."
  (let (process-file-side-effects)
    (vc-got-command t 0 nil "diff" (vc-switches 'got 'diff) "--" obj1 obj2)))

(defun vc-got--unstage (file-or-directory)
  "Unstage all the staged hunks at or within FILE-OR-DIRECTORY.
If it's nil, unstage every staged changes across the entire work
tree."
  (vc-got-command t 0 file-or-directory "unstage"))

(defun vc-got--remove (file &optional force keep-local)
  "Use got to remove FILE.
If FORCE is non-nil perform the operation even if a file contains
local modification.  If KEEP-LOCAL is non-nil keep the affected
files on disk."
  (vc-got-with-worktree (or file default-directory)
    (with-temp-buffer
      (vc-got-command t 0 file "remove"
                      (and force "-f")
                      (and keep-local "-k")))))

(defun vc-got--ref ()
  "Return a list of all references."
  (let ((process-file-side-effects nil)
        (re "^refs/\\(heads\\|remotes\\|tags\\)/\\(.*\\):")
        ;; hardcoding HEAD because it's always present and the regexp
        ;; won't match it.
        (table (list "HEAD")))
    (vc-got-with-worktree default-directory
      (with-temp-buffer
        (vc-got-command t 0 nil "ref" "-l")
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (push (match-string 2) table))
        table))))

(defun vc-got--branch (name)
  "Try to create and switch to the branch called NAME."
  ;; TODO: does branch change files or does it require update
  (let (process-file-side-effects)
    (vc-got-with-worktree default-directory
      (with-temp-buffer
        (vc-got-command t 0 nil "branch" name)))))


;; Backend properties

(defun vc-got-revision-granularity ()
  "Got has REPOSITORY granularity."
  'repository)

(defun vc-got-update-on-retrieve-tag ()
  "Like vc-git, vc-got don't need to buffers on `retrieve-tag'."
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
      nil                               ; got doesn't track directories
    (when (vc-find-root file ".got")
      (let ((s (vc-got-state file)))
        (not (memq s '(nil unregistered)))))))

(defun vc-got-state (file)
  "Return the current version control state of FILE.  See `vc-state'."
  (unless (file-directory-p file)
    (let (process-file-side-effects)
      ;; Manually calling got status and checking the result inline to
      ;; avoid building the data structure in vc-got--status.
      (with-temp-buffer
        ;; too noisy to use: (vc-got-command t nil file "status"))
        (when (zerop (vc-got--call "status" "--" file))
          (goto-char (point-min))
          (if (eobp)
              'up-to-date
            (vc-got--parse-status-char (char-after))))))))

(defun vc-got--dir-filter-files (files)
  "Remove ., .. and .got from FILES."
  (cl-loop for file in files
           unless (or (string= file "..")
                      (string= file ".")
                      (string= file ".got"))
           collect file))

(defun vc-got-dir-status-files (dir files update-function)
  "Build the status for FILES in DIR.
The builded result is given to the callback UPDATE-FUNCTION.  If
FILES is nil, consider all the files in DIR."
  (let* ((fs (vc-got--dir-filter-files (or files (directory-files dir))))
         ;; XXX: we call with files, which will probably be nil on the
         ;; first run, so we catch deleted, missing and edited files
         ;; in subdirectories.
         (res (vc-got--status nil dir files))
         double-check)
    (cl-loop for file in fs
             do (when (and (not (cdr (assoc file res #'string=)))
                           (not (file-directory-p file))
                           ;; if file doesn't exists, it's a
                           ;; untracked file that was removed.
                           (file-exists-p file))
                  ;; if we don't know the status of a file here, it's
                  ;; either up-to-date or ignored.  Save it for a
                  ;; double check
                  (push file double-check)))
    (cl-loop with statuses = (vc-got--status nil dir double-check)
             for file in double-check
             unless (eq 'unregistered (cadr (assoc file statuses #'string=)))
             do (push (list file 'up-to-date nil) res))
    (funcall update-function res nil)))

(defun vc-got-dir-extra-headers (dir)
  "Return a string for the `vc-dir' buffer heading for directory DIR."
  (let ((remote (vc-got-repository-url dir)))
    (concat (propertize "Repository : " 'face 'font-lock-type-face)
            (vc-got--repo-root) "\n"
            (when remote
              (concat
               (propertize "Remote URL : " 'face 'font-lock-type-face)
               (vc-got-repository-url dir) "\n"))
            (propertize "Branch     : " 'face 'font-lock-type-face)
            (vc-got--current-branch))))

(defun vc-got-dir-printer (info)
  "Pretty-printer for the vc-dir-fileinfo structure INFO."
  (let* ((isdir (vc-dir-fileinfo->directory info))
         (state (if isdir "" (vc-dir-fileinfo->state info)))
         (stage-state (vc-dir-fileinfo->extra info))
         (filename (vc-dir-fileinfo->name info)))
    (insert
     "  "
     (propertize
      (format "%c" (if (vc-dir-fileinfo->marked info) ?* ? ))
      'face 'font-lock-type-face)
     "  "
     (propertize
      (format "%-12s" state)
      'face (cond ((eq state 'up-to-date) 'font-lock-builtin-face)
                  ((memq state '(missing conflict)) 'font-lock-warning-face)
                  ((eq state 'edited) 'font-lock-constant-face)
                  (t 'font-lock-variable-name-face))
      'mouse-face 'highlight
      'keymap (vc-got--with-emacs-version<= "28.0.50"
                vc-dir-status-mouse-map))
     "   " (propertize
            (if stage-state
                (format "%c" stage-state)
              " ")
            'face (cond ((memq stage-state '(?A ?E)) 'font-lock-constant-face)
                        ((eq stage-state ?R) 'font-lock-warning-face)
                        (t 'font-lock-variable-name-face)))
     "    "
     (propertize filename
                 'face (if isdir 'font-lock-comment-delimiter-face
                         'font-lock-function-name-face)
                 'help-echo
                 (if isdir
                     (concat
                      "Directory\n"
                      "VC operations can be applied to it\n"
                      "mouse-3: Pop-up menu")
                   "File\nmouse-3: Pop-up menu")
                 'mouse-face 'highlight
                 'keymap vc-dir-filename-mouse-map))))

(defun vc-got-working-revision (file)
  "Return the last commit that touched FILE or \"0\" if it's newly added."
  (with-temp-buffer
    (when (vc-got--info file)
      (let ((pos (re-search-forward "^based on commit: " nil t)))
        (if pos
            (buffer-substring-no-properties pos (line-end-position))
          "0")))))

(defun vc-got-checkout-model (_files)
  "Return the checkout model.
Got uses an implicit checkout model for every file."
  'implicit)

(defun vc-got-mode-line-string (file)
  "Return the VC mode line string for FILE."
  (vc-got-with-worktree file
    (let ((def (vc-default-mode-line-string 'Got file)))
      (concat (substring def 0 4) (vc-got--current-branch)))))


;; state-changing functions

(defun vc-got-create-repo (_backend)
  "Create an empty repository in the current directory."
  (error "[vc-got] create-repo not implemented"))

(defun vc-got-register (files &optional _comment)
  "Register FILES, passing `vc-register-switches' to the backend command."
  (vc-got--add files))

(defalias 'vc-got-responsible-p #'vc-got-root)

(defun vc-got-unregister (file)
  "Unregister FILE."
  (vc-got--remove file t t))

(defun vc-got-checkin (files comment &optional _rev)
  "Commit FILES with COMMENT as commit message."
  (with-temp-buffer
    (vc-got-command t 0 files
                    "commit" "-m"
                    (log-edit-extract-headers
                     '(("Author" . "-A"))
                     comment))))


(defun vc-got-find-revision (file rev buffer)
  "Fill BUFFER with the content of FILE in the given revision REV."
  (with-current-buffer buffer
    (vc-got-with-worktree file
      (vc-got--cat rev (file-relative-name file)))))

(defun vc-got-checkout (_file &optional _rev)
  "Checkout revision REV of FILE.
If REV is t, checkout from the head."
  (error "[vc-got] checkout not implemented"))

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

(defun vc-got--proc-filter (proc s)
  "Custom output filter for async process PROC.
It's like `vc-process-filter' but supports \r inside S."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let ((buffer-undo-list t)
                (inhibit-read-only t))
            (goto-char (process-mark proc))
            (if (not (string-match ".*\r\\(.*\\)" s))
                (insert s)
              ;; handle \r
              (end-of-line)
              (let ((end (point)))
                (beginning-of-line)
                (delete-region (point) end))
              (insert (match-string 1 s)))
            (set-marker (process-mark proc) (point))))))))

(defun vc-got--push-pull (cmd op prompt)
  "Execute CMD OP, or prompt the user if PROMPT is non-nil."
  (let ((buffer (format "*vc-got : %s*" (expand-file-name default-directory))))
    (when-let (cmd (if prompt
                       (split-string
                        (read-shell-command (format "%s %s command: " cmd op)
                                            (format "%s %s " cmd op))
                        " " t)
                     (list cmd op)))
      (apply #'vc-do-async-command buffer default-directory cmd)
      (with-current-buffer buffer
        (vc-compilation-mode 'got)
        (let ((comp-cmd (mapconcat #'identity cmd " "))
              (proc (get-buffer-process buffer)))
          (setq-local compile-command comp-cmd)
          (setq-local compilation-directory default-directory)
          (setq-local compilation-arguments (list comp-cmd
                                                  nil
                                                  (lambda (_ign) buffer)
                                                  nil))
          ;; Setup a custom process filter that handles \r.
          (set-process-filter proc #'vc-got--proc-filter)))
      (vc-set-async-update buffer))))

;; TODO: this could be expanded.  After a pull the worktree needs to
;; be updated, either with a ``got update -b branch-name'' or ``got
;; update -b remote/branchname'' plus a rebase.
(defun vc-got-pull (prompt)
  "Execute a fetch prompting for the full command if PROMPT is not nil."
  (vc-got--push-pull vc-got-program "fetch" prompt))

(defun vc-got-push (prompt)
  "Execute a send prompting for the full command if PROMPT is not nil."
  (vc-got--push-pull vc-got-program "send" prompt))


;; History functions

(defun vc-got-print-log (files buffer &optional _shortlog start-revision limit)
  "Insert the revision log for FILES into BUFFER.
LIMIT limits the number of commits, optionally starting at
START-REVISION."
  (vc-setup-buffer buffer)
  (with-current-buffer buffer
    (let ((worktree-path (vc-got-root default-directory))
          (inhibit-read-only t))
      (dolist (file files)
        (vc-got--log (file-relative-name file worktree-path)
                     limit
                     start-revision)))))

(defun vc-got-log-outgoing (buffer remote-location)
  "Fill BUFFER with the diff between the local worktree branch and REMOTE-LOCATION."
  (vc-setup-buffer buffer)
  (let ((rl (vc-got-next-revision
             nil
             (if (or (not remote-location) (string-empty-p remote-location))
                 (concat "origin/" (vc-got--current-branch))
               remote-location)))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (vc-got--log nil nil nil rl))))

(defun vc-got-log-incoming (buffer remote-location)
  "Fill BUFFER with the incoming diff from REMOTE-LOCATION.
That is, the diff between REMOTE-LOCATION and the local repository."
  (vc-setup-buffer buffer)
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

(define-derived-mode vc-got-log-view-mode log-view-mode "Got-Log-View"
  "Got-specific log-view mode.
Heavily inspired by `vc-git-log-view-mode'."
  (require 'add-log)
  (setq-local log-view-file-re regexp-unmatchable)
  (setq-local log-view-per-file-logs nil)
  (setq-local log-view-message-re "^commit +\\([0-9a-z]+\\)")
  (setq-local log-view-font-lock-keywords
              (append
               `((,log-view-message-re (1 'change-log-acknowledgment)))
               ;; Handle the case:
               ;; user: foo@bar
               '(("^from: \\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)"
                  (1 'change-log-email))
                 ;; Handle the case:
                 ;; user: FirstName LastName <foo@bar>
                 ("^from: \\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
                  (1 'change-log-name)
                  (2 'change-log-email))
                 ("^date: \\(.+\\)" (1 'change-log-date))))))

;; TODO: async
;; TODO: return 0 or 1
(defun vc-got-diff (files &optional rev1 rev2 buffer _async)
  "Insert into BUFFER (or *vc-diff*) the diff for FILES from REV1 to REV2."
  (let* ((buffer (get-buffer-create (or buffer "*vc-diff*")))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (vc-got-with-worktree (or (car files)
                                default-directory)
        (cond ((and (null rev1)
                    (null rev2))
               (vc-got--diff-files files))
              ((and (null rev1)
                    rev2)
               ;; TODO: this includes the whole diff while to respect
               ;; the vc semantics we should filter only the diff for
               ;; files in FILES.
               ;;
               ;; XXX: this includes also the commit message, I
               ;; consider it a feature over the usual vc behaviour of
               ;; showing only the diff.
               (vc-got--log nil 1 rev2 nil nil nil t))
              ;;
              ;; TODO: if rev1 is nil, diff from the current version until
              ;; rev2.
              ;;
              ;; TODO 2: if rev2 is nil as well, diff against an empty
              ;; tree (i.e. get the patch from `got log -p rev1')
              (t (vc-got--diff-objects rev1 rev2)))))))

(defun vc-got-revision-completion-table (_files)
  "Return a completion table for existing revisions.
Ignores FILES because GoT doesn't have the concept of ``file
revisions''; instead, like with git, you have tags and branches."
  (letrec ((table (lazy-completion-table
                   table (lambda () (vc-got--ref)))))
    table))

(defun vc-got-annotate-command (file buf &optional rev)
  "Show annotated contents of FILE in buffer BUF.  If given, use revision REV."
  (let (process-file-side-effects)
    (with-current-buffer buf
      ;; FIXME: vc-ensure-vc-buffer won't recognise this buffer as managed
      ;; by got unless vc-parent-buffer points to a buffer managed by got.
      ;; investigate why this is needed.
      (setq-local vc-parent-buffer (find-file-noselect file))
      (apply #'vc-got-command t 0 file "blame"
             (when rev (list "-c" rev))))))

(defconst vc-got--annotate-re
  (concat "^[0-9]\\{1,\\}) " ; line number followed by )
          "\\([a-z0-9]+\\) " ; SHA-1 of commit
          "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) " ; year-mm-dd
          "\\([^ ]\\)+ ")    ; author
  "Regexp to match annotation output lines.
Provides capture groups for:
1. revision id
2. date of commit
3. author of commit")

(defconst vc-got--commit-re "^commit \\([a-z0-9]+\\)"
  "Regexp to match commit lines.
Provides capture group for the commit revision id.")

(defun vc-got-annotate-time ()
  "Return the time of the next line of annotation at or after point.
Value is returned as floating point fractional number of days."
  ;; XXX: to behave like vc-git here we should call re-search-forward
  ;; instead of looking-at, as it makes the fontification of the line
  ;; start AFTER the info.  The problem is, due to the format of the
  ;; blame, it produces an ugly result, with colors starting at
  ;; different offsets depending on how long the committer name is.
  (when (looking-at vc-got--annotate-re)
    (let ((str (match-string-no-properties 2)))
      (vc-annotate-convert-time
       (encode-time 0 0 0
                    (string-to-number (substring str 8 10))
                    (string-to-number (substring str 5 7))
                    (string-to-number (substring str 0 4)))))))

(defun vc-got-annotate-extract-revision-at-line ()
  "Return revision corresponding to the current line or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at vc-got--annotate-re)
      (match-string-no-properties 1))))


;; Tag system

(defun vc-got--tag-callback (tag)
  "`log-edit' callback for `vc-got-create-tag'.
Creates the TAG using the content of the current buffer."
  (interactive)
  (let ((msg (buffer-substring-no-properties (point-min)
                                             (point-max))))
    (with-temp-buffer
      (vc-got-command t 0 nil "tag"
                      "-m"
                      (log-edit-extract-headers nil msg)
                      "--"
                      tag))))

(defun vc-got-create-tag (_dir name branchp)
  "Attach the tag NAME to the state of the worktree.
DIR is ignored (tags are global, not per-file).  If BRANCHP is
true, NAME should create a new branch otherwise it will pop-up a
`log-edit' buffer to provide the tag message."
  ;; TODO: vc recommends to ensure that all of the files are in a clean
  ;; state, but is it useful?
  (if branchp
      (vc-got--branch name)
    (let ((buf (get-buffer-create "*vc-got tag*")))
      (with-current-buffer buf
        (erase-buffer)
        (save-excursion
          (insert "Summary: tag " name "\n\n"))
        (move-end-of-line 1)
        (switch-to-buffer buf)
        (log-edit (lambda ()
                    (interactive)
                    (unwind-protect
                        (vc-got--tag-callback name)
                      (kill-buffer buf))))))))

(defun vc-got-retrieve-tag (dir name _update)
  "Switch to the tag NAME for files at or below DIR."
  (let ((default-directory dir))
    (vc-got--update name dir)))


;; Miscellaneous

(defun vc-got-find-ignore-file (file)
  "Return the gitignore file that controls FILE."
  (expand-file-name ".gitignore"
                    (vc-got-root file)))

(defun vc-got-previous-revision (file rev)
  "Return the revision number that precedes REV for FILE or nil."
  (with-temp-buffer
    (vc-got--log file 2 rev nil nil t)
    (goto-char (point-min))
    (keep-lines "^commit")
    (when (looking-at vc-got--commit-re)
      (match-string-no-properties 1))))

(defun vc-got-next-revision (file rev)
  "Return the revision number that follows REV for FILE or nil."
  (with-temp-buffer
    (vc-got--log file nil nil rev)
    (keep-lines "^commit" (point-min) (point-max))
    (goto-char (point-max))
    (forward-line -1)    ; return from empty line to last actual commit
    (unless (= (point) (point-min))
      (forward-line -1)
      (when (looking-at vc-got--commit-re)
        (match-string-no-properties 1)))))

(defun vc-got-delete-file (file)
  "Delete FILE locally and mark it deleted in work tree."
  (vc-got--remove file t))

(defun vc-got-find-file-hook ()
  "Activate `smerge-mode' if there is a conflict."
  ;; just like vc-git-find-file-hook
  (when (and buffer-file-name
             (eq (vc-state buffer-file-name 'Got) 'conflict)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^<<<<<<< " nil 'noerror)))
    (smerge-start-session)
    (vc-message-unresolved-conflicts buffer-file-name)))

(defun vc-got-conflicted-files (dir)
  "Return the list of files with conflicts in directory DIR."
  (let* ((root (vc-got-root dir))
         (default-directory root)
         (process-file-side-effects nil))
    (cl-loop for (file status _) in (vc-got--status "C" ".")
             when (and (eq status 'conflict)
                       (file-in-directory-p file dir))
             collect file)))

(defun vc-got-repository-url (_file &optional remote-name)
  "Return URL for REMOTE-NAME, or for \"origin\" if nil."
  (let* ((default-directory (vc-got--repo-root))
         (remote-name (or remote-name "origin"))
         (heading (concat "[remote \"" remote-name "\"]"))
         (conf (cond ((file-exists-p ".git/config") ".git/config")
                     ((file-exists-p ".git")        nil)
                     ((file-exists-p "config")      "config")))
         found)
    (when conf
      (with-temp-buffer
        (insert-file-contents conf)
        (goto-char (point-min))
        (when (search-forward heading nil t)
          (forward-line)
          (while (and (not found)
                      (looking-at ".*=") ; too broad?
                      (not (= (point) (point-max))))
            (when (looking-at ".*url = \\(.*\\)")
              (setq found (match-string-no-properties 1)))
            (forward-line))
          found)))))



;; Automatically register the backend and add ".got" to the exclusion
;; list.

;;;###autoload
(add-to-list 'vc-handled-backends 'Got)

;;;###autoload
(add-to-list 'vc-directory-exclusion-list ".got")

(provide 'vc-got)
;;; vc-got.el ends here
