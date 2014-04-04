(require-package 'ess)

(autoload 'R-mode "ess-site")
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

(provide 'init-ess)
