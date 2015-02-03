
(autoload 'R-mode "ess-site")

(setq ess-smart-S-assign-key ";")

(after 'ess-mode
  (ess-add-style
   'km
   '((ess-first-continued-statement-offset . 4)
     (ess-continued-statement-offset . 0)
     (ess-arg-function-offset-new-line . nil)
     (ess-arg-function-offset . nil)
     (ess-expression-offset . nil)
     (ess-indent-level . 4)
     (ess-brace-offset . 0)
     (ess-else-offset . 0)
     (ess-close-brace-offset . 0))))
(setq ess-default-style 'km)

(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

(provide 'init-ess)
