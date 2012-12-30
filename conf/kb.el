;; kb.el - key bindings
;; evil-specific ones are in evil.el

(global-set-key (kbd "C-x \\") 'align-regexp)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
