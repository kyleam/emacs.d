;;; km-buffers.el --- Buffer-related extensions

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

;;;###autoload
(defun km/save-and-kill-buffer ()
  "Save current buffer and then kill it."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

;;;###autoload
(defun km/kill-buffer (&optional arg)
  "Kill this buffer.
With single C-u, prompt for buffer to kill.  With double C-u,
kill this buffer and the window."
  (interactive "P")
  (cond
   ((not arg)
    (kill-buffer))
   ((equal arg '(16))
    (kill-buffer-and-window))
   (t
    (call-interactively #'kill-buffer))))

;;;###autoload
(defun km/save-buffers ()
  "Run `save-some-buffers', but don't ask to save the current buffer.
`save-some-buffers' is called interactively."
  (interactive)
  (let* ((base-buf (buffer-base-buffer))
         (buf (or base-buf (current-buffer)))
         (buf-file (buffer-file-name buf)))
    (when (and (buffer-live-p buf)
               (buffer-modified-p buf)
               buf-file)
      (with-current-buffer buf
        (save-buffer))))
  (call-interactively #'save-some-buffers))

(provide 'km-buffers)
;;; km-buffers.el ends here
