(require-package 'paredit)

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(eval-after-load 'paredit
  '(progn
    (define-key paredit-mode-map (kbd "C-,") 'paredit-forward-slurp-sexp)
    (define-key paredit-mode-map (kbd "C-.") 'paredit-forward-barf-sexp)
    (define-key paredit-mode-map (kbd "C-M-,") 'paredit-backward-slurp-sexp)
    (define-key paredit-mode-map (kbd "C-M-.") 'paredit-backward-barf-sexp)))

(provide 'init-elisp)