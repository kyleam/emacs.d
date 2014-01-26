;; Make cite key have form <last author last name><year><first word>.
(setq bibtex-autokey-year-length 4)
(setq bibtex-autokey-titleword-length nil)
(setq bibtex-autokey-titlewords-stretch 0)
(setq bibtex-autokey-titlewords 1)
(setq bibtex-autokey-year-title-separator "")

(setq bibtex-autokey-titleword-ignore
      '("A" "An" "On" "The" "Why" "How" "Where" "[0-9].*"))

;; Used by `bibtex-fill-entry'.
(setq bibtex-align-at-equal-sign t)

(provide 'init-bib)
