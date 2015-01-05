
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq reftex-default-bibliography '("refs.bib"))

(put 'LaTeX-narrow-to-environment 'disabled nil)

(provide 'init-tex)
