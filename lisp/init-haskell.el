
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(after 'haskell-process
  ;; Unbind `haskell-process-cabal' from user's key.
  (define-key interactive-haskell-mode-map (kbd "C-c c") nil))

(provide 'init-haskell)
