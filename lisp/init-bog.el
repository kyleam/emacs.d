(setq bog-keymap-prefix (kbd "C-c b"))

(autoload 'bog-mode "bog")

(add-hook 'org-mode-hook 'bog-mode)

(provide 'init-bog)
