(setq inhibit-splash-screen t
      initial-scratch-message nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

;; Line info
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(load-theme 'stekene-light t)

(custom-theme-set-faces
 'stekene-light
 '(default ((t (:background "white"))))
 '(outline-1 ((t (:inherit font-lock-function-name-face))))
 '(outline-2 ((t (:inherit font-lock-doc-face))))
 '(outline-3 ((t (:inherit font-lock-constant-face))))
 '(org-todo ((t (:foreground "#ba2727" :background "gray85"))))
 '(org-done ((t (:foreground "#557755" :background "gray85"))))
 '(org-date ((t (:inherit font-lock-builtin-face))))
 '(org-upcoming-deadline ((t (:inherit font-lock-constant-face))))
 '(org-warning ((t (:inherit font-lock-string-face))))
 '(org-agenda-date-today ((t (:weight semi-bold))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
 '(org-agenda-structure ((t (:foreground "gray40"))))
 '(org-agenda-date ((t (:inherit default))))
 '(org-scheduled ((t (:inherit font-lock-string-face))))
 '(org-agenda-calendar-event ((t (:inherit font-lock-function-name-face))))
 '(gnus-group-mail-1-empty ((t (:inherit font-lock-string-face))))
 '(gnus-group-mail-2-empty ((t (:inherit font-lock-function-name-face))))
 '(gnus-group-mail-3-empty ((t (:inherit font-lock-type-face))))
 '(gnus-group-mail-1 ((t (:inherit gnus-group-mail-1-empty :weight semi-bold))))
 '(gnus-group-mail-2 ((t (:inherit gnus-group-mail-2-empty :weight semi-bold))))
 '(gnus-group-mail-3 ((t (:inherit gnus-group-mail-3-empty :weight semi-bold))))
 '(gnus-subject ((t (:inherit font-lock-doc-face))))
 '(gnus-button ((t (:inherit default))))
 '(gnus-cite-1 ((t (:inherit font-lock-function-name-face))))
 '(gnus-cite-2 ((t (:inherit font-lock-string-face))))
 '(gnus-header-name ((t (:inherit font-lock-constant-face))))
 '(gnus-header-content ((t (:inherit default))))
 '(term-color-black ((t (:inherit default))))
 '(term-color-red ((t (:inherit font-lock-string-face))))
 '(term-color-green ((t (:foreground "#3cb371"))))
 '(term-color-yellow ((t (:inherit font-lock-doc-face))))
 '(term-color-blue ((t (:inherit font-lock-function-name-face))))
 '(term-color-magenta ((t (:foreground "#8b008b"))))
 '(term-color-cyan ((t (:foreground "#008b8b"))))
 ;; Darken a bit (was gray40).
 '(font-lock-keyword-face ((t (:foreground "gray33")))))

(custom-set-faces
 '(dired-subtree-depth-1-face ((t (:background "gray80"))))
 '(dired-subtree-depth-2-face ((t (:background "gray90"))))
 '(dired-subtree-depth-3-face ((t (:background "gray95")))))

(provide 'init-appearance)
