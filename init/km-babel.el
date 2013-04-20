;; set up babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '( (perl . t)
    (sh . t)
    (python . t)
    (R . t)
    (emacs-lisp . t)
    (latex . t)
    ))
;; don't ask for confirmation before running code
(setq org-confirm-babel-evaluate nil)

;; babel minted latex export
;; modified from
;; http://orgmode.org/worg/org-tutorials/org-latex-export.html
(setq org-export-latex-listings 'minted)
(setq org-export-latex-custom-lang-environments
      '(
        (R "rcode")
        (sh "shcode")
        (python "pythoncode")
        ))
;; (setq org-export-latex-minted-options
;;       '(("frame" "lines")
;;         ("fontsize" "\\scriptsize")
;;         ("linenos" "")))
(setq org-latex-to-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
