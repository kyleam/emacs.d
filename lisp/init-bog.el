(setq bog-keymap-prefix (kbd "C-c b"))

(setq bog-completing-read 'ido-completing-read
      bog-read-file-name 'ido-read-file-name)

(autoload 'bog-mode "bog")

(add-hook 'org-mode-hook 'bog-mode)

(provide 'init-bog)
