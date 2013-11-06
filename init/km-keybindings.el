;; keybindings that don't go with other topics

(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-c i i") 'indent-relative)

(global-set-key (kbd "C-x K") 'kill-buffer-and-window)

;; overrides `suspend-emacs' (which is also bound to C-x C-z)
(global-set-key (kbd "C-z") 'zap-to-char)
(global-unset-key (kbd "M-z"))

(global-set-key (kbd "C-'") 'backward-kill-word)

(global-set-key (kbd "C-c r s") 'query-replace)
(global-set-key (kbd "C-c r S") 'replace-string)
(global-set-key (kbd "C-c r r") 'query-replace-regexp)
(global-set-key (kbd "C-c r R") 'replace-regexp)

(global-set-key (kbd "C-c c t") 'km/todo-comment)

;; multiple cursors
;; put under insert prefix
(global-set-key (kbd "C-c i m l") 'mc/edit-lines)
(global-set-key (kbd "C-c i m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c i m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c i m a") 'mc/mark-all-like-this)

(global-set-key (kbd "C-;") 'er/expand-region)
