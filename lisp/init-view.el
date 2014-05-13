(key-chord-define-global ",v" 'view-mode)

(setq view-read-only t)

(eval-after-load 'view
  '(progn
     (define-key view-mode-map "l" 'recenter-top-bottom)
     (define-key view-mode-map "a" 'ace-jump-mode)
     (diminish 'view-mode "Vw")))

(provide 'init-view)
