;;; init-god.el --- God mode configuration

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

(require 'god-mode)

(add-to-list 'god-exempt-predicates #'km/god-gnus-p)

(add-hook 'view-mode-hook (lambda ()
                            (if view-mode
                                (god-local-mode-pause)
                              (god-local-mode-resume))))

(add-hook 'org-capture-mode-hook (lambda () (god-local-mode -1)))

(add-hook 'god-mode-enabled-hook (lambda ()
                                   (when view-mode
                                     (view-mode -1))
                                   (when (derived-mode-p 'emacs-lisp-mode)
                                     (lispy-mode -1))))

(add-hook 'god-mode-disabled-hook (lambda ()
                                    (when (derived-mode-p 'emacs-lisp-mode)
                                      (lispy-mode 1))))

(add-hook 'god-mode-enabled-hook 'km/god-update-cursor)
(add-hook 'god-mode-disabled-hook 'km/god-update-cursor)

(defun km/god-update-cursor ()
  (setq cursor-type (if god-local-mode 'bar 'box)))

(defun km/god-gnus-p ()
  "Return non-nil if a Gnus-related mode is enabled."
  (derived-mode-p 'gnus-group-mode
                  'gnus-summary-mode
                  'gnus-article-mode
                  'message-mode))

(global-set-key (kbd "C-c d") 'god-local-mode)

(define-key god-local-mode-map "." 'repeat)
(define-key god-local-mode-map "i" 'god-local-mode)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(provide 'init-god)
;;; init-god.el ends here
