;;; km-files.el --- File-related extensions

;; Copyright (C) 2012-2018 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/emacs.d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'recentf)

;;;###autoload
(defun km/rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (user-error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (user-error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
;;;###autoload
(defun km/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (user-error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(declare-function dired-jump (&optional other-window file-name))
;;;###autoload
(defun km/dired-jump-file-at-point ()
  "Run `dired-jump' on file at point."
  (interactive)
  (let ((file (or (and (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end)))
                  (thing-at-point 'filename))))
    (if (and file (file-exists-p file))
        (dired-jump 'other-window (expand-file-name file))
      (user-error "No file at point"))))

;;;###autoload
(defun km/touch-buffer-file ()
  "Run touch on `buffer-file-name'."
  (interactive)
  (call-process "touch" nil nil nil
                (or (buffer-file-name (buffer-base-buffer))
                    (user-error "Not visiting file"))))

;;;###autoload
(defun km/write-file ()
  "Run `write-file'.
Use the current file name as initial input of prompt."
  (interactive)
  (let* ((init-file (and buffer-file-name
                         (file-name-nondirectory buffer-file-name)))
         (new-file (read-file-name "Write file: " nil nil nil
                                   init-file)))
    (write-file new-file t)))

;; Modified from prelude
;;;###autoload
(defun km/recentf-find-file ()
  "Find a file from `recentf-list'."
  (interactive)
  (find-file (km/read-recent-file)))

;;;###autoload
(defun km/recentf-find-file-other-window ()
  "Find a file from `recentf-list' in other window."
  (interactive)
  (find-file-other-window (km/read-recent-file)))

(defun km/read-recent-file ()
  (completing-read "Choose recent file: " recentf-list nil t))


;;; Scratch files

(defvar km/find-scratch-buffers
  '((?e ".el"  "Elisp")
    (?g ".scm" "Guile scheme")
    (?h ".hs"  "Haskell")
    (?m ".md"  "Markdown")
    (?n ""     "No mode")
    (?o ".org" "Org")
    (?p ".py"  "Python")
    (?r ".r"   "R")
    (?s ".sh"  "Shell")
    (?t ".txt" "Text"))
  "List of scratch buffers.
Format of each element should be (CHARACTER EXTENSION DOC). DOC
is not required.")

;;;###autoload
(defun km/scratch-find-file (&optional pwd)
  "Find scratch buffer.

Prompt with characters from `km/find-scratch-buffers' to
determine the extension of the scratch file.

With prefix argument PWD, find the scratch file in
`default-directory' instead of /tmp."
  (interactive "P")
  (switch-to-buffer (km/scratch--find-file-no-select pwd)))

;;;###autoload
(defun km/scratch-find-file-other-window (&optional pwd)
    "Like `km/find-scratch-file', but open buffer in another window."
  (interactive "P")
  (switch-to-buffer-other-window (km/scratch--find-file-no-select pwd)))

(defun km/scratch--find-file-no-select (pwd)
  (find-file-noselect (km/scratch--get-file-name pwd)))

(defun km/scratch--get-file-name (pwd)
  (let* ((choices (mapcar #'car km/find-scratch-buffers))
         (ch (read-char-choice (concat "[" choices "]") choices))
         (ext (cadr (assq ch km/find-scratch-buffers))))
    (concat (if pwd default-directory "/tmp/") "scratch" ext)))

(provide 'km-files)
;;; km-files.el ends here
