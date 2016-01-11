;;; km-buffer-cleanup.el --- Clean up buffer on save

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

(require 'whitespace)

(defvar-local km/prevent-cleanup nil
  "If set, `km/cleanup-buffer' does not perform clean up on save.")

(defun km/toggle-prevent-cleanup ()
  "Toggle state of `km/prevent-cleanup'."
  (interactive)
  (if km/prevent-cleanup
      (progn
        (message "Allowing cleanup on save")
        (kill-local-variable 'whitespace-style)
        (global-whitespace-mode 0)
        (global-whitespace-mode 1))
    (message "Preventing cleanup on save")
    (setq-local whitespace-style
         '(face trailing indentation
           tab-mark space-mark newline-mark))
    (global-whitespace-mode 0)
    (global-whitespace-mode 1))
  (setq km/prevent-cleanup (not km/prevent-cleanup)))

(defun km/cleanup-buffer ()
  (interactive)
  (unless km/prevent-cleanup
    (whitespace-cleanup)
    (delete-trailing-whitespace)))

(provide 'km-buffer-cleanup)
;;; km-buffer-cleanup.el ends here
