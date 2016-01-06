;;; init-buffers.el --- Buffer-related configuration

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

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)

(setq ibuffer-expert t
      ibuffer-restore-window-config-on-quit t
      ibuffer-show-empty-filter-groups nil)

(defun km/save-and-kill-buffer ()
  "Save current buffer and then kill it."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

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

(global-set-key (kbd "C-x k") 'km/kill-buffer)

(key-chord-define-global "js" 'km/save-buffers)

;; Replace `list-buffers' with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-buffers)
;;; init-buffers.el ends here
