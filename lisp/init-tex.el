(require-package 'auctex)

(require 'tex)
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;; Add path for pdflatex.
(setenv "PATH"
        (concat
         "/usr/texbin" ":"
         (getenv "PATH")))

(defun km/org-mode-reftex-setup ()
  (define-key org-mode-map (kbd "C-c [") 'reftex-citation))

(add-hook 'org-mode-hook 'km/org-mode-reftex-setup)

(setq reftex-default-bibliography '("~/refs/refs.bib"))

(provide 'init-tex)
