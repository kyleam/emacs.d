
(setq reftex-default-bibliography '("refs.bib"))

(setq font-latex-fontify-sectioning 'color)

(put 'LaTeX-narrow-to-environment 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.[tT]e[xX]\\'" . latex-mode))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(provide 'init-tex)
