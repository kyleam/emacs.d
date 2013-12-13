(put 'dired-find-alternate-file 'disabled nil)

(require 'dired-x)

;; .git is present as part of `dired-omit-extensions', but this
;; seems to only be taken into account if the a non-exension part
;; exists
(setq dired-omit-files (concat dired-omit-files "\\|\\.git$\\|__pycache__"))

(setq-default dired-omit-files-p t)

(setq dired-listing-switches "-alh")
