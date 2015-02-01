
(require 'yasnippet)


(key-chord-define-global ";e" 'yas-expand)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(yas-global-mode)

(provide 'init-yas)
