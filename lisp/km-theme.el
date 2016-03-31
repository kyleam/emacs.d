;;; km-theme.el --- Tweaks to the stekene light theme

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

(load-theme 'stekene-light t)

(custom-theme-set-faces
 'stekene-light
 '(default ((t (:background "white"))))
 '(diff-added ((t (:foreground "DarkGreen" :background "grey98"))))
 '(diff-context ((t (:foreground "grey50" :background "grey98"))))
 '(diff-removed ((t (:foreground "#aa2222" :background "grey98"))))
 ;; Darken a bit (was gray40).
 '(font-lock-keyword-face ((t (:foreground "gray33"))))
 '(gnus-button ((t (:inherit default))))
 '(gnus-cite-1 ((t (:inherit font-lock-function-name-face))))
 '(gnus-cite-2 ((t (:inherit font-lock-string-face))))
 '(gnus-group-mail-1 ((t (:inherit gnus-group-mail-1-empty :weight semi-bold))))
 '(gnus-group-mail-1-empty ((t (:inherit font-lock-string-face))))
 '(gnus-group-mail-2 ((t (:inherit gnus-group-mail-2-empty :weight semi-bold))))
 '(gnus-group-mail-2-empty ((t (:inherit font-lock-function-name-face))))
 '(gnus-group-mail-3 ((t (:inherit gnus-group-mail-3-empty :weight semi-bold))))
 '(gnus-group-mail-3-empty ((t (:inherit font-lock-type-face))))
 '(gnus-header-content ((t (:inherit default))))
 '(gnus-header-name ((t (:inherit font-lock-constant-face))))
 '(gnus-subject ((t (:inherit font-lock-doc-face))))
 '(magit-diff-added ((t (:foreground "DarkGreen" :background "grey98"))))
 '(magit-diff-added-highlight ((t (:foreground "DarkGreen" :background "grey98"))))
 '(magit-diff-context ((t (:foreground "grey50" :background "grey98"))))
 '(magit-diff-context-highlight ((t (:foreground "grey50" :background "grey98"))))
 '(magit-diff-hunk-heading ((t (:foreground "gray30" :background "grey90"))))
 '(magit-diff-hunk-heading-highlight ((t (:foreground "gray30" :background "grey80"))))
 '(magit-diff-removed ((t (:foreground "#aa2222" :background "grey98"))))
 '(magit-diff-removed-highlight ((t (:foreground "#aa2222" :background "grey98"))))
 '(magit-section-highlight ((t (:background "white"))))
 '(org-agenda-calendar-event ((t (:inherit font-lock-function-name-face))))
 '(org-agenda-date ((t (:inherit default))))
 '(org-agenda-date-today ((t (:weight semi-bold))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
 '(org-agenda-structure ((t (:foreground "gray40"))))
 '(org-date ((t (:inherit font-lock-builtin-face))))
 '(org-done ((t (:foreground "#557755" :background "gray85"))))
 '(org-scheduled ((t (:inherit org-scheduled-today))))
 '(org-todo ((t (:foreground "#ba2727" :background "gray85"))))
 '(org-upcoming-deadline ((t (:inherit font-lock-constant-face))))
 '(org-warning ((t (:inherit font-lock-string-face))))
 '(outline-1 ((t (:inherit font-lock-function-name-face))))
 '(outline-2 ((t (:inherit font-lock-doc-face))))
 '(outline-3 ((t (:inherit font-lock-constant-face))))
 '(term-color-black ((t (:inherit default))))
 '(term-color-blue ((t (:inherit font-lock-function-name-face))))
 '(term-color-cyan ((t (:foreground "#008b8b"))))
 '(term-color-green ((t (:foreground "#3cb371"))))
 '(term-color-magenta ((t (:foreground "#8b008b"))))
 '(term-color-red ((t (:inherit font-lock-string-face))))
 '(term-color-yellow ((t (:inherit font-lock-doc-face)))))

(custom-set-faces
 '(dired-subtree-depth-1-face ((t (:background "gray80"))))
 '(dired-subtree-depth-2-face ((t (:background "gray90"))))
 '(dired-subtree-depth-3-face ((t (:background "gray95"))))
 '(Info-quoted ((t (:inherit default))))
 '(whitespace-indentation ((t (:background "gray90")))))

(provide 'km-theme)
;;; km-theme.el ends here
