(defvar init-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path init-lisp-dir)
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

(require 'init-elpa)
(require 'init-appearance)
(require 'init-general)

(require-package 'dash)
(require 'dash)
(require-package 's)
(require 's)
(require-package 'mocker)

(require 'init-org)
(require 'init-orgcontacts)
(require 'init-babel)

(require 'init-buffile)
(require 'init-framewin)

(require 'init-editing)
(require-package 'boxquote)

(require 'init-text)
(require 'init-elisp)
(require 'init-shell)
(require 'init-haskell)
(require 'init-python)
(require 'init-ess)
(require-package 'lua-mode)

(require 'init-tex)
(require 'init-bib)
(require 'init-bog)

(require-package 'htmlize)
(require-package 'less-css-mode)
(require-package 'yaml-mode)

(require 'init-dired)
(require 'init-git)
(require 'init-projectile)
(require 'init-grep)

(require 'init-ido)
(require 'init-smex)

(require 'init-external)

(require 'init-yas)

(require 'init-gnus)

(when (file-exists-p (expand-file-name "init-untracked.el" init-lisp-dir))
  (require 'init-untracked))

(require 'server)
(unless (server-running-p)
  (server-start))
