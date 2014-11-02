(require-package 'paredit)

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(after 'paredit
  (define-key paredit-mode-map (kbd "C-,") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-.") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-M-,") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-M-.") 'paredit-backward-barf-sexp)

  ;; Don't let `paredit-splice-sexp' override default 'M-s' map.
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-p") 'paredit-splice-sexp))

(provide 'init-elisp)
