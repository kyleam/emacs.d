
(require 'yasnippet)

(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(key-chord-define-global ";e" 'yas-expand)

(diminish 'yas-minor-mode)

(provide 'init-yas)
