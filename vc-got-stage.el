;;; vc-got-stage.el --- Stage functionalities for vc-got  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Omar Polo

;; Author: Omar Polo <op@omarpolo.com>
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

;;; Commentary:

;; Stage-related functions for vc-got.  This allows vc-got to stage
;; and commit individual chunks and not entire filesets.

;;; Code:

(require 'log-edit)
(require 'rx)
(require 'vc)

(defvar vc-got-program)                 ;vc-got.el
(declare-function vc-got--diff    "vc-got")
(declare-function vc-got--unstage "vc-got" (file))
(declare-function vc-got--status  "vc-got" (status-codes dir &rest files))
(declare-function vc-got-checkin  "vc-got" (fileset comment))
(declare-function vc-got-root     "vc-got" (dir-or-file))

(defvar vc-got-stage--process nil
  "The got stage process.")

(defvar vc-got-stage--fileset nil
  "Remaining fileset to process.")

(defun vc-got-stage--assert-proc ()
  "Assert no vc-got-stage process is running."
  (when (process-live-p vc-got-stage--process)
    (error "A vc-got-stage-files is already in progress")))

(defun vc-got-stage-files (fileset)
  "Interactively stage hunks from files in FILESET."
  (interactive (list (cadr (vc-deduce-fileset))))
  (vc-got-stage--assert-proc)
  (if (not fileset)
      (message "[vc-got] nothing to stage.")
    (setq vc-got-stage--fileset fileset)
    (vc-got-stage--next)))

(defun vc-got-stage--next ()
  "Process next file in stage list."
  (vc-got-stage--assert-proc)
  (let ((file (car vc-got-stage--fileset)))
    (if (not file)
        (progn (kill-buffer (process-buffer vc-got-stage--process))
               (message "[vc-got] stage done."))
      (setq vc-got-stage--fileset (cdr vc-got-stage--fileset))
      (let ((buf (get-buffer-create "*vc-got-stage*")))
        (pop-to-buffer buf)
        (with-current-buffer buf
          (buffer-disable-undo)
          (erase-buffer)
          (read-only-mode)
          (unless (derived-mode-p 'diff-mode)
            (diff-mode)))
        (setq vc-got-stage--process
              (make-process :name "got"
                            :buffer buf
                            :command (list vc-got-program "stage" "-p" file)
                            :connection 'pty
                            :filter #'vc-got-stage--filter
                            :sentinel #'vc-got-stage--sentinel))))))

(defun vc-got-stage--kill-separators ()
  "Kill the separator lines in interactive got stage."
  (save-excursion
    (forward-line -2)
    (kill-line)
    (goto-char (point-min))
    (kill-line)))

(defun vc-got-stage--filter (proc string)
  "Filter for got stage process.
PROC is the process, STRING part of its output."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (let ((inhibit-read-only t))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert string)
          (save-excursion
            (beginning-of-line)
            (when (looking-at (rx bol
                                  (group (zero-or-one "un")
                                         "stage"
                                         (zero-or-more anychar)
                                         "?")))
              (let ((msg (match-string 1)))
                (kill-line)             ; kill the question
                (vc-got-stage--kill-separators)
                (process-send-string buf
                                     (condition-case nil
                                         (if (y-or-n-p msg) "y\n" "n\n")
                                       (quit "q\n")))
                (erase-buffer)))))))))

(defun vc-got-stage--sentinel (_proc event)
  "Sentinel for got stage process.
Should be only called when EVENT is finished."
  (when (string= event "finished\n")
    (vc-got-stage--next)))

;; TODO: make this interactive just as stage is
(defun vc-got-stage-unstage (fileset)
  "Unstage staged hunks in FILESET."
  (interactive (list (cadr (vc-deduce-fileset))))
  (vc-got-stage--assert-proc)
  (if fileset
      (dolist (file fileset)
        (vc-got--unstage file))
    (vc-got--unstage nil)))

(defun vc-got-stage-diff (fileset)
  "Pop a buffer with the staged diff for FILESET.
If FILESET is nil, show the diff for every staged hunks."
  (interactive (list (cadr (vc-deduce-fileset))))
  (with-current-buffer (get-buffer-create "*vc-diff*")
    (pop-to-buffer (current-buffer))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (diff-mode)
      (if fileset
          (dolist (file fileset)
            (vc-got--diff "-s" file))
        (vc-got--diff "-s")))))

(defun vc-got-stage-commit ()
  "Commit staged hunks."
  (interactive)
  (let* ((default-directory (vc-got-root default-directory))
         (buf (get-buffer-create "*vc-got-stage-commit*"))
         (status (vc-got--status "M" "."))
         (staged-files (cl-loop for (file _ staged) in status
                                when staged
                                collect file)))
    (pop-to-buffer buf)
    (log-edit (lambda ()
                (interactive)
                (let ((msg (buffer-substring-no-properties (point-min)
                                                           (point-max))))
                  (kill-buffer)
                  (vc-got-checkin nil msg)))
              t
              `((log-edit-listfun . ,(lambda ()
                                       (mapcar #'file-relative-name
                                               staged-files)))))))

(provide 'vc-got-stage)
;;; vc-got-stage.el ends here
