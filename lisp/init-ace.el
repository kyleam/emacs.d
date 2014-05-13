(require-package 'ace-jump-mode)
(require-package 'ace-link)

(key-chord-define-global ";a" 'ace-jump-mode)

(setq ace-jump-mode-scope 'frame)

(ace-link-setup-default)

(provide 'init-ace)
