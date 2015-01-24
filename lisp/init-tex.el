
(setq reftex-default-bibliography '("refs.bib"))

(put 'LaTeX-narrow-to-environment 'disabled nil)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(provide 'init-tex)
