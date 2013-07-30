(require 'yasnippet)

(yas-reload-all)

(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))

(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (yas-minor-mode)))
