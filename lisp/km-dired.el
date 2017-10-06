;;; km-dired.el --- Dired extensions

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
(require 'dired)
(require 'org)
(require 'km-util)
(require 'projectile)

(defun km/dired-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (km/dired-completing-buffer)))

;;;###autoload
(defun km/dired-switch-to-buffer-other-window ()
  (interactive)
  (pop-to-buffer (km/dired-completing-buffer)))

(defun km/dired-completing-buffer ()
  (completing-read "Dired buffer: "
                   (mapcar #'buffer-name (km/mode-buffers 'dired-mode))))

;;;###autoload
(defun km/org-open-dired-marked-files (&optional arg)
  "Open marked files (or next ARG) with `org-open-file'."
  (interactive "p")
  (setq arg (and current-prefix-arg arg))
  (let* ((files (dired-get-marked-files nil arg))
         (num-files (length files)))
    (when (or (< num-files 5)
              (yes-or-no-p (format "Open %s files?" num-files)))
      (dolist (f files) (org-open-file f)))))

;;;###autoload
(defun km/dired-view-file-other-window ()
  "In Dired, view this file in another window."
  (interactive)
  (view-file-other-window (dired-get-file-for-visit)))

;;;###autoload
(defun km/dired-copy-and-edit ()
  "Copy file and enter `wdired-mode' for completing rename."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Must be in a Dired buffer"))
  (let* ((fname (dired-get-filename))
         (flag "---copy---")
         (new-fname (concat fname flag)))
    (when (file-directory-p fname)
      (user-error "File cannot be directory"))
    (copy-file fname new-fname)
    (dired-revert)
    (wdired-change-to-wdired-mode)
    (goto-char (point-min))
    (re-search-forward (format "%s\\(%s\\)"
                               (file-name-nondirectory fname)
                               flag))
    (replace-match "" t nil nil 1)))

;;;###autoload
(defun km/dired-touch-deref (&optional arg)
  "Run 'touch -h' on makred files (or next ARG)."
  (interactive "p")
  (setq arg (and current-prefix-arg arg))
  (--when-let (dired-get-marked-files nil arg)
    (apply #'call-process "touch" nil nil nil "-h" it)))

;;;###autoload
(defun km/dired-copy-last-mtime-as-kill ()
  "Copy last modification time for file at point."
  (interactive)
  (--when-let (dired-get-filename nil 'noerror)
    (kill-new (message "%s" (format-time-string
                             "%Y%m%d%H%M.%S"
                             (nth 5 (file-attributes it)))))))

;;;###autoload
(defun km/dired-beginning-of-buffer ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 1))


;;; Copying file names

;;;###autoload
(defun km/dired-copy-project-filename-as-kill ()
  "Copy names of marked project files into kill ring.
This is similar to `dired-copy-filename-as-kill', but the leading
path is always relative to `projectile-project-root'."
  (interactive)
  (km/dired-copy-filename-relative-to-directory
   (projectile-project-root)))

;;;###autoload
(defun km/dired-copy-relative-filename-as-kill (&optional arg)
  "Copy names of marked (or next ARG) files into kill ring.
This is similar to `dired-copy-filename-as-kill', but the leading
path is always relative to the `default-directory' of the other
window."
  (interactive "p")
  (setq arg (and current-prefix-arg arg))
  (km/dired-copy-filename-relative-to-directory
   (km/other-default-directory) arg))

(defun km/dired-copy-filename-relative-to-directory (directory &optional arg)
  "Like `dired-copy-filename-as-kill', but the filename is always
relative to DIRECTORY."
  (let* ((string
          (mapconcat #'identity
                     (mapcar (lambda (f) (file-relative-name f directory))
                             (dired-get-marked-files t arg))
                     " ")))
    (if (eq last-command 'kill-region)
        (kill-append string nil)
      (kill-new string))
    (message "%s" string)))

(defun km/other-default-directory ()
  "Get `default-directory' for result of `(other-window 1)'."
  (save-window-excursion
    (other-window 1)
    default-directory))

(provide 'km-dired)
;;; km-dired.el ends here
