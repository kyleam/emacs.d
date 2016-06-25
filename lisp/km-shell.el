;;; km-shell.el --- Shell-related extensions

;; Copyright (C) 2012-2016 Kyle Meyer <kyle@kyleam.com>

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

(require 'comint)
(require 'dash)
(require 'term)

(defvar km/terminal "urxvt")

;;;###autoload
(defun km/open-external-terminal ()
  (interactive)
  (start-process "ext-term" nil km/terminal))

;;;###autoload
(defun km/zsh-ansi-term (&optional directory name)
  "Open an ansi-term buffer running ZSH in DIRECTORY.

If a terminal for DIRECTORY already exists, switch to that
buffer.  If the current buffer is a terminal for DIRECTORY,
create an additional terminal.

By default, DIRECTORY is `default-directory'.

With a numeric prefix argument 0, prompt the user with existing
ZSH terminal directories.

With a C-u prefix argument, set DIRECTORY to the user home
directory.

With any other non-nil value, prompt for a directory.

If NAME is non-nil, use *NAME* for the buffer name instead of
*zsh: DIRECTORY*.  If that buffer already exists, it will be
grabbed regardless of whether its default-directory matches
DIRECTORY."
  (interactive (km/zsh-ansi-term--args))
  (let* ((dir (abbreviate-file-name directory))
         (name (or name (concat "zsh: " dir)))
         (full-name (concat "*" name "*"))
         (default-directory dir))
    (unless (and (not (string= (km/zsh-ansi-term-directory) dir))
                 (get-buffer full-name))
      (cl-letf (((symbol-function 'switch-to-buffer)
                 (lambda (&rest _) nil)))
        (ansi-term "zsh" name)))
    (pop-to-buffer-same-window full-name)
    (comint-goto-process-mark)))

;;;###autoload
(defun km/zsh-toggle-ansi-term-home (&optional other-window)
  (interactive "P")
  (if (string= "*zsh*" (buffer-name))
      (bury-buffer)
    (let ((display-buffer-overriding-action
           (and other-window '(nil (inhibit-same-window . t)))))
      (km/zsh-ansi-term "~/" "zsh"))))

;;;###autoload
(defun km/zsh-ansi-term-other-window (&optional directory)
  (interactive (km/zsh-ansi-term--args))
  (let ((display-buffer-overriding-action
         '(nil (inhibit-same-window . t))))
    (km/zsh-ansi-term directory)))

(defun km/zsh-ansi-term--args ()
  (list (cond
         ((not current-prefix-arg)
          default-directory)
         ((= (prefix-numeric-value current-prefix-arg) 4)
          "~/")
         ((= (prefix-numeric-value current-prefix-arg) 0)
          (let ((dirs (km/zsh-ansi-term-current-directories)))
            (cl-case (length dirs)
              (0 (user-error "No ZSH buffers found"))
              (1 (car dirs))
              (t (completing-read "Directory: " dirs
                                  nil nil nil nil (car dirs))))))
         (t
          (read-directory-name "Directory: ")))))

(defun km/zsh-ansi-term-directory (&optional buffer)
  "Return directory name for ZSH terminal in BUFFER.
BUFFER defaults to current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((bname (buffer-name)))
      (and (derived-mode-p 'term-mode)
           (string-match "^\\*zsh: \\(.*\\)\\*\\(<[0-9]+>\\)*$"
                         bname)
           (match-string 1 bname)))))

(defun km/zsh-ansi-term-current-directories ()
  (-distinct (-keep #'km/zsh-ansi-term-directory (buffer-list))))

(provide 'km-shell)
;;; km-shell.el ends here
