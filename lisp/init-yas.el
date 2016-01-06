;;; init-yas.el --- Yasnippet configuration

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

(require 'yasnippet)

(setq yas-fallback-behavior nil)

(defvar km/personal-snippets
  (file-name-as-directory (expand-file-name "psnippets" user-emacs-directory)))

(when (file-exists-p km/personal-snippets)
  (add-to-list 'yas-snippet-dirs km/personal-snippets))

(defun km/yas-with-comment (str)
  (concat comment-start
          (unless (s-ends-with? " " comment-start) " ")
          str comment-end))

(define-key yas-minor-mode-map (kbd "C-c i") 'yas-expand)
;; Remove commands with 'C-c &' prefix, which conflicts with
;; `org-mark-ring-goto' binding'
(define-key yas-minor-mode-map (kbd "C-c &") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(yas-global-mode)

(provide 'init-yas)
