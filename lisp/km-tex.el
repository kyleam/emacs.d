;;; km-tex.el --- TeX extensions

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

(require 'tex)
(require 'latex)

(defun km/latex-imenu-create-index-function ()
  ;; See `LaTeX-imenu-create-index-function'.
  (TeX-update-style)
  (let ((sec-re (LaTeX-outline-regexp))
        (title-re "\\*\\{0,1\\}{\\([^}]+\\)}")
        entries)
    (goto-char (point-min))
    (while (re-search-forward sec-re nil t)
      (let ((sec (replace-regexp-in-string
                  "\\\\" "" (match-string-no-properties 0)))
            (title (and (looking-at title-re)
                        (replace-regexp-in-string
                         "\\s-\\s-+" " "
                         (replace-regexp-in-string
                          "\n" "" (match-string-no-properties 1))))))
        (when (> (length title) 45)
          (setf (substring title 21 -21) "..."))
        (push (cons (if title (format "%s (%s)" title sec) sec)
                    (save-excursion (beginning-of-line) (point-marker)))
              entries)))
    (nreverse entries)))

(provide 'km-tex)
;;; km-tex.el ends here
