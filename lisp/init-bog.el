(add-to-list 'load-path "~/src/emacs/bog")

(setq bog-keymap-prefix (kbd "C-c b"))
(global-set-key (kbd "C-c B") 'bog-commander)

(autoload 'bog-commander "bog")
(autoload 'bog-mode "bog")

(add-hook 'org-mode-hook 'bog-mode)

(setq bog-use-citekey-cache t)

(provide 'init-bog)
