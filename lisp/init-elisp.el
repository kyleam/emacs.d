;;; init-elisp.el --- Elisp-related configuration

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

(setq lispy-no-permanent-semantic t)

(add-hook 'emacs-lisp-mode-hook 'lispy-mode)
(add-hook 'emacs-lisp-mode-hook 'km/elisp-set-outline-vars)
;; This likely breaks lispy's outline functions, but I don't use them.
(add-hook 'lispy-mode-hook 'km/elisp-set-outline-vars)

(after 'lispy
  ;; This is ugly, but I haven't found another way to stop
  ;; `imenu-create-index-function' from being set to
  ;; `semantic-create-imenu-index'.  Trying to set it in
  ;; `emacs-lisp-mode-hook' or `lispy-mode-hook' doesn't work.
  (defalias 'semantic-create-imenu-index 'imenu-default-create-index-function))

(defun km/elisp-outline-level ()
  (and (looking-at (concat "^" outline-regexp))
       (- (match-end 0) (match-beginning 0) 3)))

(defun km/elisp-set-outline-vars ()
  (setq outline-regexp ";;;;* ")
  (setq outline-level 'km/elisp-outline-level))

(provide 'init-elisp)
;;; init-elisp.el ends here
