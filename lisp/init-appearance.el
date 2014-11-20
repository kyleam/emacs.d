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
 ;; Darken a bit (was gray40).
 '(font-lock-keyword-face ((t (:foreground "gray33")))))

(custom-set-faces
 '(dired-subtree-depth-1-face ((t (:background "gray80"))))
 '(dired-subtree-depth-2-face ((t (:background "gray90"))))
 '(dired-subtree-depth-3-face ((t (:background "gray95")))))

(provide 'init-appearance)
