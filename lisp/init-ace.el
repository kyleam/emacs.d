(require-package 'ace-jump-mode)
(require-package 'ace-link)
(require-package 'ace-window)

(key-chord-define-global ";a" 'ace-jump-mode)

(setq ace-jump-mode-scope 'frame)

(ace-link-setup-default)

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c m o") 'ace-link-org))

(define-key window-map "a" 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(provide 'init-ace)
