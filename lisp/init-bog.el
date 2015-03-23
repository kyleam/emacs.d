(add-to-list 'load-path "~/src/emacs/bog/")
(require 'bog-autoloads)

(setq bog-subdirectory-group 2
      bog-use-citekey-cache t)

(setq bog-keymap-prefix (kbd "C-c b"))

(add-hook 'org-mode-hook 'bog-mode)

(global-set-key bog-keymap-prefix bog-command-map)

(provide 'init-bog)
