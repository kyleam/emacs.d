;;; km-avy.el --- Extensions for avy

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

(require 'avy)
(require 'cl-macs)

(defmacro km/avy-after-goto (&rest body)
  (declare (indent defun) (debug (body)))
  (let ((result (cl-gensym)))
    `(let ((,result
            (let ((avy-all-windows nil))
              (call-interactively #'avy-goto-subword-1))))
       (when (integerp ,result)
         ,@body))))

(declare-function occur-mode-display-occurrence "replace")
;;;###autoload
(defun km/occur-avy-goto-subword-1 ()
  "Like `avy-goto-subword-1', but display occurence."
  (interactive)
  (km/avy-after-goto (occur-mode-display-occurrence)))

(declare-function compilation-display-error "compile")
;;;###autoload
(defun km/grep-avy-goto-subword-1 ()
  "Like `avy-goto-subword-1', but call `compilation-display-error'."
  (interactive)
  (km/avy-after-goto (compilation-display-error)))

(declare-function org-agenda-do-context-action "org-agenda")
;;;###autoload
(defun km/org-agenda-avy-goto-subword-1 ()
  (interactive)
  (km/avy-after-goto (org-agenda-do-context-action)))

(declare-function magit-diff-show-or-scroll-up "magit-diff")
;;;###autoload
(defun km/magit-avy-goto-subword-1 ()
  "Like `km/avy-goto-subword-1', but maybe show commit and limit to window."
  (interactive)
  (km/avy-after-goto
    (when (derived-mode-p 'magit-log-mode)
      (magit-diff-show-or-scroll-up))))

(declare-function gnus-summary-scroll-up "gnus-sum" (lines))
;;;###autoload
(defun km/gnus-avy-goto-subword-and-select ()
  (interactive)
  (km/avy-after-goto (gnus-summary-scroll-up 0)))

(declare-function elfeed-search-show-entry "elfeed-search" (entry))
;;;###autoload
(defun km/elfeed-avy-goto-subword-1 ()
  (interactive)
  (km/avy-after-goto
    (call-interactively #'elfeed-search-show-entry)))

(provide 'km-avy)
;;; km-avy.el ends here
