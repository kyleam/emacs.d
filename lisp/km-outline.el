;;; init-outline.el --- Outline mode extensions

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

(require 'outline)

(defun km/outline--parent-levels (n)
  "Return list of parent levels.
4 -> (1 2 3)
1 -> nil"
  (unless (> n 0)
    (user-error "N must be positive"))
  (let (parents)
    (while (/= n 1)
      (setq n (1- n))
      (push n parents))
    parents))

(defun km/outline-imenu-create-index ()
  "Generate Imenu index for headings.
This assumes that `outline-regexp' marks the characters that
indicate a heading, but not the text following it.  The heading
text is taken as everything on the line after the
`outline-regexp' match (except for any leading whitespace)."
  (goto-char (point-min))
  (let (path-alist                      ; ((level . heading))
        index)
    (while (re-search-forward (concat "^" outline-regexp) nil t)
      (let* ((head (replace-regexp-in-string
                    "\\`\\s-*" ""
                    (buffer-substring-no-properties (point) (point-at-eol))))
             (head-level (save-excursion (beginning-of-line)
                                         (funcall outline-level)))
             (parent-levels (km/outline--parent-levels head-level))
             (parent-heads
              (delq nil (mapcar (lambda (n) (cdr (assoc n path-alist)))
                                parent-levels)))
             (target (concat (mapconcat #'identity
                                        (append parent-heads
                                                (list head))
                                        " / "))))
        (unless (string= head "")
          (push (cons target (save-excursion (beginning-of-line) (point-marker)))
                index))
        (push (cons head-level head) path-alist)))
    (nreverse index)))

(declare-function helm-imenu "helm-imenu")
;;;###autoload
(defun km/outline-jump-to-heading ()
  "Jump to heading specified by `outline-regexp'."
  (interactive)
  (let ((imenu-create-index-function #'km/outline-imenu-create-index)
        helm-cached-imenu-tick
        helm-cached-imenu-alist
        helm-cached-imenu-candidates)
    (call-interactively #'helm-imenu)))

(provide 'km-outline)
;;; km-outline.el ends here
