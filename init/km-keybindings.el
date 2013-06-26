;; keybindings that don't go with other topics

(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-c i") 'indent-relative)

(global-set-key (kbd "C-x K") 'kill-buffer-and-window)

;; overrides `suspend-emacs' (which is also bound to C-x C-z)
(global-set-key (kbd "C-z") 'zap-to-char)
(global-unset-key (kbd "M-z"))
