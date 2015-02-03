(setq view-read-only t)

(after 'view
  (define-key view-mode-map "l" 'recenter-top-bottom)
  (define-key view-mode-map "f" 'forward-word)
  (define-key view-mode-map "b" 'backward-word)
  (define-key view-mode-map "]" 'forward-paragraph)
  (define-key view-mode-map "[" 'backward-paragraph)
  (define-key view-mode-map "j" 'ace-jump-mode))

(key-chord-define-global "hq" 'view-mode)

(provide 'init-view)
