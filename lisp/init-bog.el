(add-to-list 'load-path "~/src/emacs/bog/")
(require 'bog-autoloads)

(setq bog-keymap-prefix (kbd "C-c b")
      bog-subdirectory-group 2
      bog-use-citekey-cache t)

(add-hook 'org-mode-hook 'bog-mode)

(global-set-key (kbd "C-c B") 'bog-commander)

(provide 'init-bog)
