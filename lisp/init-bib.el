;; Make cite key have form <last author last name><year><first word>.
(setq bibtex-autokey-year-length 4
      bibtex-autokey-titleword-length nil
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titlewords 1
      bibtex-autokey-year-title-separator ""
      bibtex-autokey-titleword-ignore '("A" "An" "On" "The"  "[0-9].*"))

(setq bibtex-align-at-equal-sign t)  ; Used by `bibtex-fill-entry'.

(eval-after-load 'bibtex
  '(setq bibtex-entry-format
         (append '(realign sort-fields) bibtex-entry-format)))

(provide 'init-bib)
