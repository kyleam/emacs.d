(define-prefix-command 'replace-map)
(global-set-key (kbd "C-c r") 'replace-map)

(define-prefix-command 'kill-map)
(global-set-key (kbd "C-c k") 'kill-map)

(define-prefix-command 'insert-map)
(global-set-key (kbd "C-c i") 'insert-map)

;; multiple cursors
;; Put under insert prefix.
(define-prefix-command 'multiple-cursors-map)
(define-key insert-map "m" 'multiple-cursors-map)

(define-prefix-command 'external-map)
(global-set-key (kbd "C-c x") 'external-map)

(provide 'init-keymaps)
