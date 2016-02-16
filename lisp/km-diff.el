;;; km-diff.el --- Diff-related extensions

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

(require 'dash)
(require 'diff)
(require 'ediff)

;;;###autoload
(defun km/diff-lock-buffer ()
  "Rename current diff buffer to include new file name."
  (interactive)
  (rename-buffer
   (format "*Diff: %s*"
           (abbreviate-file-name
            (substring-no-properties (diff-find-file-name))))))

(defun km/diff--with-other-window (diff-func)
  (let ((windows (window-list)))
       (unless (= (length windows) 2)
         (user-error "Function restricted to two-window frames"))
       (-if-let* ((file-a (buffer-file-name
                             (window-buffer (car windows))))
                  (file-b (buffer-file-name
                           (window-buffer (cadr windows)))))
           (funcall diff-func file-a file-b)
         (user-error "At least one buffer is not visiting a file"))))

;;;###autoload
(defun km/diff-with-other-window ()
  "Run `diff' on current window's file and other window's file."
  (interactive)
  (km/diff--with-other-window #'diff))

;;;###autoload
(defun km/ediff-with-other-window ()
  "Run `ediff' on current window's file and other window's file."
  (interactive)
  (km/diff--with-other-window #'ediff))

(defvar km/ediff-previous-window-config nil)

;;;###autoload
(defun km/ediff-save-window-config ()
  (setq km/ediff-previous-window-config (current-window-configuration)))

;;;###autoload
(defun km/ediff-restore-window-config ()
  (when km/ediff-previous-window-config
    (set-window-configuration km/ediff-previous-window-config)
    (setq km/ediff-previous-window-config nil)))

(provide 'km-diff)
;;; km-diff.el ends here
