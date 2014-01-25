(put 'dired-find-alternate-file 'disabled nil)

(require 'dired-x)

;; .git is present as part of `dired-omit-extensions', but this
;; seems to only be taken into account if the a non-exension part
;; exists.
(setq dired-omit-files
      (concat dired-omit-files "\\|\\.git$\\|\\.gitignore$\\|__pycache__"))

(defvar km/latex-omit-extensions '(".aux"
                                   ".fdb_latexmk"
                                   ".fls"
                                   ".log"
                                   ".nav"
                                   ".out"
                                   ".snm")
  "Intermediate LaTeX files")

(setq dired-omit-extensions
      (append dired-omit-extensions km/latex-omit-extensions))

(setq-default dired-omit-files-p t)

(setq dired-listing-switches "-alh")
