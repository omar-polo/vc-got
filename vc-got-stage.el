;;; vc-got-stage.el --- Stage changes diff buffers   -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'subr-x))

(defgroup vc-got-stage nil
  "Stage hunks in vc-got diff buffers"
  :group 'faces
  :prefix "vc-got-stage-")

(defface vc-got-stage-staged-face
    '((t (:foreground "red" :background "yellow")))
  "Face used to highlight the staged mark on changes."
  :group 'vc-got-stage)

(defvar vc-got-stage-fileset nil
  "The files diffed in the last call to `vc-got-diff'.")

(defvar vc-got-stage-overlay-priority 0
  "Specify overlay priority.
Higher values means higher priority.  DON'T use negative numbers.")

(defvar vc-got-stage--overlays nil
  "The list of overlays.")

(defvar vc-got-stage-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "A") #'vc-got-stage-apply)
    (define-key map (kbd "U") #'vc-got-unstage-all)
    (define-key map (kbd "b") #'vc-got-stage-beginning-of-change)
    (define-key map (kbd "e") #'vc-got-stage-end-of-change)
    (define-key map (kbd "n") #'vc-got-stage-next-change)
    (define-key map (kbd "p") #'vc-got-stage-prev-change)
    (define-key map (kbd "t") #'vc-got-stage-toggle-mark)
    map)
  "Keymap for function `vc-got-stage-mode'.")

;;;###autoload
(define-minor-mode vc-got-stage-mode
  "Stage hunks in vc-got diff buffers.

\\{vc-got-stage-mode-map}"
  :group vc-got-stage
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c g") vc-got-stage-prefix-map)
            map))

;;;###autoload (defun vc-got-stage-activate ()
;;;###autoload   "Activate vg-got-stage-mode if the current buffer is a vc-got diff."
;;;###autoload   (when-let (root (vc-find-root default-directory ".got"))
;;;###autoload     (vc-got-stage-mode +1)))

(defun vc-got-stage-activate ()
  "Activate vg-got-stage-mode if the current buffer is a vc-got diff."
  (message "VC got stage activate? %s" (vc-find-root default-directory ".got"))
  (when-let (root (vc-find-root default-directory ".got"))
    (vc-got-stage-mode +1)))

;;;###autoload (add-hook 'diff-mode-hook #'vc-got-stage-activate)
(add-hook 'diff-mode-hook #'vc-got-stage-activate)

(defun vc-got-stage--in-change ()
  "T if the point is in a line that's part of a change."
  (save-excursion
    (beginning-of-line)
    (when-let (ch (char-after))
      (or (= ch ?\-)
          (= ch ?\+)))))

(defun vc-got-stage--change-marked-p ()
  "T if the current change is marked."
  (let ((p (point)))
    (cl-loop
       for overlay in vc-got-stage--overlays
       if (and (overlay-start overlay)
               (= p (overlay-start overlay)))
       return t
       finally (return nil))))

(defun vc-got-stage--compute-y-or-n (buf)
  "Fill BUF with ``y'' or ``n'' lines for staging purpose."
  (save-excursion
    (goto-char (point-min))
    (let ((p (point)))
      (while (not (= p (progn (vc-got-stage-next-change)
                              (point))))
        (setq p (point))
        (if (vc-got-stage--change-marked-p)
            (with-current-buffer buf
              (insert "y\n"))
          (with-current-buffer buf
            (insert "n\n")))))))

(defun vc-got-stage-unstage-all ()
  (interactive)
  (let* ((default-directory (vc-find-root default-directory ".got"))
         (unstage-buf       (get-buffer-create "*vc-got-unstage*")))
    (unless (zerop (apply #'process-file "got" nil unstage-buf nil
                          "unstage" (mapcar #'file-relative-name
                                            vc-got-stage-fileset)))
      (pop-to-buffer unstage-buf)
      (error "Got unstage failed"))
    (kill-buffer unstage-buf)))

(defun vc-got-stage--apply-impl (script tmp-file)
  "Apply the stages using SCRIPT as script (TMP-FILE is the path)."
  (let* ((default-directory (vc-find-root default-directory ".got"))
         (stage-buf         (get-buffer-create "*vc-got-stage*")))
    (vc-got-stage-unstage-all)
    (vc-got-stage--compute-y-or-n script)
    (with-current-buffer script
      (save-buffer))
    (unless (zerop (apply #'process-file "got" nil stage-buf nil "stage" "-p"
                          "-F" tmp-file (mapcar #'file-relative-name
                                                vc-got-stage-fileset)))
      (pop-to-buffer stage-buf)
      (error "Got stage failed"))
    (kill-buffer stage-buf)))

(defun vc-got-stage-apply ()
  "Apply the stages.
This will first reset the stages of all the involved files, then
stage the marked changes."
  (interactive)
  (let* ((tmp-file (make-temp-file "vc-got-stage-script"))
         (script   (find-file-noselect tmp-file)))
    (unwind-protect
         (vc-got-stage--apply-impl script tmp-file)
      (kill-buffer script)
      (delete-file tmp-file))))

(defun vc-got-stage-beginning-of-change ()
  "Goto the beginning of the current change."
  (interactive)
  (ignore-errors
    (beginning-of-line)
    (while (vc-got-stage--in-change)
      (forward-line -1))
    (forward-line)))

(defun vc-got-stage-end-of-change ()
  "Goto the end of the current change."
  (interactive)
  (ignore-errors
    (beginning-of-line)
    (while (vc-got-stage--in-change)
      (forward-line))
    (forward-line -1)))

(defun vc-got-stage--prevnext-change (n)
  "Goto next/previous change by N."
  (let ((start (point)))
    (beginning-of-line)
    (while (and (not (= (point) (if (= n -1)
                                    (point-min)
                                  (point-max))))
                (vc-got-stage--in-change))
      (forward-line n))
    (while (let ((face (get-text-property (point) 'face)))
             (and (not (= (point) (if (= n -1)
                                      (point-min)
                                    (point-max))))
                  (or (eq face 'diff-hunk-header)
                      (eq face 'diff-header)
                      (eq face 'diff-context))))
      (forward-line n))
    (if (= n -1)
        (vc-got-stage-beginning-of-change))
    (unless (vc-got-stage--in-change)
      (goto-char start)
      (message "No prev/next change"))))

(defun vc-got-stage-prev-change ()
  "Goto previous change."
  (interactive)
  (vc-got-stage--prevnext-change -1))

(defun vc-got-stage-next-change ()
  "Goto next change."
  (interactive)
  (vc-got-stage--prevnext-change +1))

(defun vc-got-stage--delete-overlay-at (point)
  "Delete overlays at POINT.
Return t if something was deleted."
  (let (delp)
    (cl-delete-if (lambda (overlay)
                    (let ((start (overlay-start overlay)))
                      ;; silently drop dangling overlays
                      (cond ((not start)
                             t)
                            ((= point start)
                             (delete-overlay overlay)
                             (setq delp t)))))
                  vc-got-stage--overlays)
    delp))

(defun vc-got-stage-toggle-mark ()
  "Toggle the staged status on the change at point."
  (interactive)
  (when (vc-got-stage--in-change)
    (save-excursion
      (vc-got-stage-beginning-of-change)
      (unless (vc-got-stage--delete-overlay-at (point))
        (let ((overlay (make-overlay (point) (point))))
          (overlay-put overlay
                       'before-string
                       (propertize "A"
                                   'display '(left-fringe right-triangle)
                                   'face    'vc-got-stage-staged-face))
          (push overlay vc-got-stage--overlays))))))

(provide 'vc-got-stage)
;;; vc-got-stage.el ends here
