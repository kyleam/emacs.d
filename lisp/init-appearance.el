(setq default-frame-alist '((font . "Droid Sans Mono-9"))
      inhibit-splash-screen t
      initial-scratch-message nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

;; Line info
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(require-package 'diminish)
(require 'diminish)
(diminish 'abbrev-mode "Ab")
;; Other minor modes (paredit, view, yas, ...) are diminished where they
;; are setup.

(provide 'init-appearance)
