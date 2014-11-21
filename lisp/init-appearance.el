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
 ;; Darken a bit (was gray40).
 '(font-lock-keyword-face ((t (:foreground "gray33")))))

(custom-set-faces
 '(dired-subtree-depth-1-face ((t (:background "gray80"))))
 '(dired-subtree-depth-2-face ((t (:background "gray90"))))
 '(dired-subtree-depth-3-face ((t (:background "gray95")))))

(provide 'init-appearance)
