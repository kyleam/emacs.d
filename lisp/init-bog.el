(setq bog-keymap-prefix (kbd "C-c b"))
(global-set-key (kbd "C-c B") 'bog-commander)

(autoload 'bog-commander "bog")
(autoload 'bog-mode "bog")

(add-hook 'org-mode-hook 'bog-mode)

(provide 'init-bog)
