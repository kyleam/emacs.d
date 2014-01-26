;;; Shell and configuration files

(require-package 'pkgbuild-mode)

(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("PKGBUILD" . pkgbuild-mode))

(provide 'init-shell)
