(require-package 'ace-jump-mode)
(require-package 'ace-link)
(require-package 'ace-window)

(key-chord-define-global ";a" 'ace-jump-mode)

(setq ace-jump-mode-scope 'frame)

(ace-link-setup-default)

(define-key window-map "a" 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(provide 'init-ace)
