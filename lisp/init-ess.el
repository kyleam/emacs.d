;;; init-ess.el --- ESS configuration

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

(autoload 'R-mode "ess-site")

(setq ess-smart-S-assign-key ";")

(setq ess-use-ido nil)

(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

(define-abbrev-table 'ess-mode-abbrev-table
  '(("true" "TRUE")
    ("false" "FALSE"))
  :system t)

(dolist (hook '(ess-mode-hook inferior-ess-mode-hook))
  (add-hook hook (lambda ()
                   (setq local-abbrev-table ess-mode-abbrev-table)))
  (add-hook hook 'abbrev-mode))

(defun km/ess-eval-buffer-up-to-line ()
  "Send up to the current line to inferior ESS process."
  (interactive)
  (ess-eval-region (point-min) (line-end-position) nil))

(defvar km/ess-dplry-pipe-key "|")

(defun km/ess-insert-dplyr-pipe ()
  "Insert `km/ess-dplry-pipe' using `ess-smart-S-assign'.
Based on instructions in `ess-smart-S-assign-key', I didn't think
this would work, but it seems to so far."
  (interactive)
  (let ((ess-S-assign " %>% ")
        (ess-smart-S-assign-key km/ess-dplry-pipe-key))
    (call-interactively #'ess-smart-S-assign)))

(after 'ess-mode
  (define-key ess-mode-map (kbd "C-c C-.") 'km/ess-eval-buffer-up-to-line)
  (define-key ess-mode-map "|" 'km/ess-insert-dplyr-pipe))

(after 'ess-inf
  (define-key inferior-ess-mode-map "|" 'km/ess-insert-dplyr-pipe))

(provide 'init-ess)
;;; init-ess.el ends here
