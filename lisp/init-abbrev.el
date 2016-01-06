;;; init-abbrev.el --- Abbrev mode configuration

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

(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'prog-mode-hook 'abbrev-mode)

(define-abbrev-table 'typo-abbrev-table nil)
(abbrev-table-put global-abbrev-table :parents
                  (cons typo-abbrev-table
                        (abbrev-table-get global-abbrev-table :parents)))

(defun km/abbrev-add-case-global ()
  "Define lower abbreviation for the word before point.
Like `add-global-abbrev', but always make the abbreviation the
lower case variant of the word before point."
  (interactive)
  ;; Modified from `add-abbrev'.
  (let* ((table global-abbrev-table)
         (exp (buffer-substring-no-properties
               (point)
               (save-excursion (forward-word -1) (point))))
         (name (downcase exp)))
    (when (or (not (abbrev-expansion name table))
              (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                                name (abbrev-expansion name table))))
      (define-abbrev table name exp))))

(defun km/abbrev-inverse-add-uppercase-global ()
  "Define uppercase expansion for the word before point.
Like `inverse-add-global-abbrev', but always use the lower case
version of the word before point as the abbreviation and the
upper case version as the expansion."
  (interactive)
  ;; Modified from `inverse-add-abbrev'.
  (let* ((table global-abbrev-table)
         (end (point))
         (start (save-excursion (forward-word -1) (point)))
         (name (downcase (buffer-substring-no-properties start end)))
         (exp (upcase name)))
    (when (or (not (abbrev-expansion name table))
              (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                                name (abbrev-expansion name table))))
      (define-abbrev table name exp)
      (save-excursion
        (goto-char end)
        (expand-abbrev)))))

(define-key abbrev-map "c" 'km/abbrev-add-case-global)
(define-key abbrev-map "iu" 'km/abbrev-inverse-add-uppercase-global)

(provide 'init-abbrev)
;;; init-abbrev.el ends here
