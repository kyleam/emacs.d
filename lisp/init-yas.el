(require-package 'yasnippet)

(require 'yasnippet)
(yas-reload-all)

(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)

(provide 'init-yas)
