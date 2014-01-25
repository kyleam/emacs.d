;; Set up babel languages.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (python . t)
   (R . t)
   (emacs-lisp . t)
   (latex . t)))

;; Don't ask for confirmation before running code.
(setq org-confirm-babel-evaluate nil)

;; Babel minted latex export
;; Modified from
;; http://orgmode.org/worg/org-tutorials/org-latex-export.html.
(setq org-export-latex-listings 'minted)
(setq org-export-latex-custom-lang-environments
      '((R "rcode")
        (sh "shcode")
        (python "pythoncode")))

(setq org-latex-to-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
