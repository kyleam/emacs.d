;; check km-ui.el if don't find something in here

(setq-default fill-column 72)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; ess
;; (require 'ess-site)
(add-hook 'ess-mode-hook (lambda ()
                           (setq ess-indent-level 4)))

;; shell scripts
(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 4)))

(global-set-key (kbd "C-x \\") 'align-regexp)

;; make whitespace-mode use just basic coloring
;; http://ergoemacs.org/emacs/whitespace-mode.html
(setq whitespace-style (quote
                        (spaces tabs newline space-mark
                                tab-mark newline-mark)))

(delete-selection-mode t) ;; write over selected text
(transient-mark-mode t)
;; share clipboard with system
(setq x-select-enable-clipboard t)

(blink-cursor-mode -1)

(put 'narrow-to-region 'disabled nil)
