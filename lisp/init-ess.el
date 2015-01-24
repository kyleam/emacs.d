
(autoload 'R-mode "ess-site")

(setq ess-smart-S-assign-key ";")

(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

(provide 'init-ess)
