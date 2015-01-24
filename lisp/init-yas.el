
(require 'yasnippet)

(yas-global-mode)

(key-chord-define-global ";e" 'yas-expand)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(provide 'init-yas)
