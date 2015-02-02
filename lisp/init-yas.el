
(require 'yasnippet)

(setq yas-fallback-behavior nil)

(define-key yas-minor-mode-map (kbd "C-c i") 'yas-expand)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(yas-global-mode)

(provide 'init-yas)
