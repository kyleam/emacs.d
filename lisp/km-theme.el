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
 '(match ((t (:inherit default :background "PaleGreen1"))))
 '(compilation-info ((t (:inherit font-lock-function-name-face))))
 '(diff-added ((t (:foreground "DarkGreen" :background "grey98"))))
 '(diff-context ((t (:foreground "grey50" :background "grey98"))))
 '(diff-removed ((t (:foreground "#aa2222" :background "grey98"))))
 '(dired-symlink ((t (:inherit font-lock-constant-face))))
 ;; Darken a bit (was gray40).
 '(font-lock-keyword-face ((t (:foreground "gray33"))))
 '(font-latex-math-face ((t (:inherit font-lock-builtin-face))))
 '(font-latex-string-face ((t (:inherit default))))
 '(git-annex-dired-annexed-available ((t (:inherit font-lock-type-face))))
 '(git-annex-dired-annexed-unavailable ((t (:inherit font-lock-string-face))))
 '(gnus-button ((t (:inherit default))))
 '(gnus-cite-1 ((t (:inherit font-lock-function-name-face))))
 '(gnus-cite-2 ((t (:inherit font-lock-string-face))))
 '(gnus-group-mail-1 ((t (:inherit gnus-group-mail-1-empty :weight bold))))
 '(gnus-group-mail-1-empty ((t (:inherit font-lock-string-face))))
 '(gnus-group-mail-2 ((t (:inherit gnus-group-mail-2-empty :weight bold))))
 '(gnus-group-mail-2-empty ((t (:inherit font-lock-function-name-face))))
 '(gnus-group-mail-3 ((t (:inherit gnus-group-mail-3-empty :weight bold))))
 '(gnus-group-mail-3-empty ((t (:inherit font-lock-type-face))))
 '(gnus-header-content ((t (:inherit default))))
 '(gnus-header-name ((t (:inherit font-lock-constant-face))))
 '(gnus-header-subject ((t (:inherit font-lock-doc-face))))
 '(gnus-subject ((t (:inherit font-lock-doc-face))))
 '(gnus-summary-normal-read ((t (:inherit font-lock-comment-face))))
 '(helm-grep-finish ((t (:inherit compilation-mode-line-exit))))
 '(helm-grep-file ((t (:inherit compilation-info))))
 '(helm-grep-lineno ((t (:inherit compilation-line-number))))
 '(helm-selection ((t (:inherit region))))
 '(helm-swoop-target-word-face ((t (:inherit lazy-highlight))))
 '(helm-swoop-target-line-block-face ((t (:inherit match))))
 '(helm-swoop-target-line-face ((t (:inherit match))))
 '(helm-match-item ((t (:inherit lazy-highlight))))
 '(isearch ((t (:inherit match))))
 '(magit-cherry-unmatched ((t (:inherit font-lock-string-face))))
 '(magit-cherry-equivalent ((t (:inherit font-lock-constant-face))))
 '(magit-diff-added ((t (:foreground "DarkGreen" :background "grey98"))))
 '(magit-diff-added-highlight ((t (:foreground "DarkGreen" :background "grey98"))))
 '(magit-diff-context ((t (:foreground "grey50" :background "grey98"))))
 '(magit-diff-context-highlight ((t (:foreground "grey50" :background "grey98"))))
 '(magit-diff-hunk-heading ((t (:foreground "gray30" :background "grey90"))))
 '(magit-diff-hunk-heading-highlight ((t (:foreground "gray30" :background "grey80"))))
 '(magit-diff-file-heading ((t (:inherit default :weight bold))))
 '(magit-diff-file-heading-selection ((t (:inherit default :weight bold))))
 '(magit-diff-removed ((t (:foreground "#aa2222" :background "grey98"))))
 '(magit-diff-removed-highlight ((t (:foreground "#aa2222" :background "grey98"))))
 '(magit-popup-argument ((t (:inherit font-lock-string-face))))
 '(magit-reflog-amend ((t (:inherit font-lock-function-name-face))))
 '(magit-reflog-checkout ((t (:inherit font-lock-builtin-face))))
 '(magit-reflog-commit ((t (:inherit default))))
 '(magit-reflog-merge ((t (:inherit font-lock-doc-face))))
 '(magit-reflog-other ((t (:inherit font-lock-type-face))))
 '(magit-reflog-rebase ((t (:inherit font-lock-string-face))))
 '(magit-reflog-remote ((t (:inherit font-lock-variable-name-face))))
 '(magit-reflog-reset ((t (:inherit font-lock-warning-face))))
 '(magit-section-highlight ((t (:background "white"))))
 '(magit-section-heading ((t (:inherit font-lock-constant-face :weight bold))))
 '(magit-section-secondary-heading ((t (:inherit default))))
 '(mm-uu-extract ((t (:background "gray95" ))))
 '(org-agenda-calendar-event ((t (:inherit font-lock-function-name-face))))
 '(org-agenda-calendar-sexp ((t (:inherit font-lock-variable-name-face))))
 '(org-agenda-date ((t (:foreground "gray40"))))
 '(org-agenda-date-today ((t (:weight bold))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
 '(org-agenda-structure ((t (:foreground "gray60"))))
 '(org-date ((t (:inherit font-lock-type-face))))
 '(org-done ((t (:foreground "#557755" :background "gray95"))))
 '(org-scheduled ((t (:inherit default))))
 '(org-scheduled-today ((t (:inherit org-scheduled))))
 '(org-todo ((t (:inherit font-lock-string-face :background "gray95"))))
 '(org-upcoming-deadline ((t (:inherit default))))
 '(org-warning ((t (:inherit font-lock-string-face))))
 '(outline-1 ((t (:inherit font-lock-doc-face))))
 '(outline-2 ((t (:inherit font-lock-function-name-face))))
 '(outline-3 ((t (:inherit font-lock-constant-face))))
 '(term-color-black ((t (:inherit default))))
 '(term-color-blue ((t (:inherit font-lock-function-name-face))))
 '(term-color-cyan ((t (:foreground "#008b8b"))))
 '(term-color-green ((t (:foreground "#3cb371"))))
 '(term-color-magenta ((t (:foreground "#8b008b"))))
 '(term-color-red ((t (:inherit font-lock-string-face))))
 '(term-color-yellow ((t (:inherit font-lock-doc-face)))))

(custom-set-faces
 '(cursor ((t (:background "#f08080"))))
 '(dired-subtree-depth-1-face ((t (:background "gray80"))))
 '(dired-subtree-depth-2-face ((t (:background "gray90"))))
 '(dired-subtree-depth-3-face ((t (:background "gray95"))))
 '(Info-quoted ((t (:inherit default))))
 '(whitespace-indentation ((t (:background "gray90"))))
 '(whitespace-trailing ((t (:background "gray90")))))

(provide 'km-theme)
;;; km-theme.el ends here
