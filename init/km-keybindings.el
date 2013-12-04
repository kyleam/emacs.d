;; keybindings that don't go with other topics

(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)

;; overrides `suspend-emacs' (which is also bound to C-x C-z)
(global-set-key (kbd "C-z") 'zap-to-char)
(global-unset-key (kbd "M-z"))

(global-set-key (kbd "C-'") 'backward-kill-word)

(define-key replace-map "s" 'query-replace)
(define-key replace-map "S" 'replace-string)
(define-key replace-map "r" 'query-replace-regexp)
(define-key replace-map "R" 'replace-regexp)

(define-key insert-map "t" 'km/todo-comment)

(define-key insert-map "i" 'indent-relative)

(define-key multiple-cursors-map "l" 'mc/edit-lines)
(define-key multiple-cursors-map "n" 'mc/mark-next-like-this)
(define-key multiple-cursors-map "p" 'mc/mark-previous-like-this)
(define-key multiple-cursors-map "a" 'mc/mark-all-like-this)

(global-set-key (kbd "C-;") 'er/expand-region)

(define-key external-map "s" 'shell-command)
