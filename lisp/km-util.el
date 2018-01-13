;;; km-util.el --- Utilities

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

(require 'dash)

;; Taken from
;; http://milkbox.net/note/single-file-master-emacs-configuration/.
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun km/mode-buffers (mode)
  (--filter (with-current-buffer it (derived-mode-p mode))
            (buffer-list)))

(defun km/region-or-buffer-line-bounds ()
  "Return line bounds for region.
If region is active, return postions that mark the beginning of
the first line and end of the last line that the region touches.
If there is no active region, return a the minimum and maximum
point in the buffer."
  (if (use-region-p)
      (list (progn (goto-char (region-beginning)) (point-at-bol))
            (progn (goto-char (region-end)) (1+ (point-at-eol))))
    (list (point-min) (point-max))))

(defun km/open-github-patch (buffer)
  "Find GitHub patch link in BUFFER and show it in a new buffer."
  (let ((url
         (with-current-buffer buffer
           (save-excursion
             (goto-char (point-min))
             (if (re-search-forward "https://github\\.com/.*\\.patch" nil t)
                 (match-string-no-properties 0)
               (user-error "No patch found"))))))
    (with-current-buffer (get-buffer-create
                          (generate-new-buffer-name "*mail-github-patch*"))
      (url-insert-file-contents url)
      (diff-mode)
      (view-mode 1)
      (pop-to-buffer (current-buffer)))))

(provide 'km-util)
;;; km-util.el ends here
