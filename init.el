(defvar km/init-lisp-dir (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path km/init-lisp-dir)

(require 'init-elpa)

(require 'cask)
(cask-initialize)
(require 'pallet)
(pallet-mode 1)

(require 'init-appearance)

(require 'cl-lib)
(require 'dash)
(require 's)

(require 'init-general)
(require 'init-diminish)

(require 'init-org)
(require 'init-orgcontacts)
(require 'init-babel)

(require 'init-files)
(require 'init-buffers)
(require 'init-framewin)
(require 'init-ace)
(require 'init-view)

(require 'init-editing)

(require 'init-text)
(require 'init-elisp)
(require 'init-shell)
(require 'init-haskell)
(require 'init-python)
(require 'init-ess)

(require 'init-tex)
(require 'init-bib)
(require 'init-bog)

(require 'init-dired)
(require 'init-git)
(require 'init-projectile)
(require 'init-snakemake)

(require 'init-ido)
(require 'init-smex)

(require 'init-external)

(require 'init-yas)

(require 'init-gnus)

(when (file-exists-p (expand-file-name "init-untracked.el" km/init-lisp-dir))
  (require 'init-untracked))

(require 'server)
(unless (server-running-p)
  (server-start))
