(defvar init-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path init-lisp-dir)
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

(require 'init-appearance)
(require 'init-elpa)
(require 'init-general)

(require-package 'dash)
(require-package 'mocker)

(require 'init-org)
(require 'init-orgcontacts)
(require 'init-babel)

(require 'init-buffile)
(require 'init-framewin)

(require 'init-editing)

(require 'init-text)
(require 'init-elisp)
(require 'init-shell)
(require 'init-haskell)
(require 'init-python)
(require-package 'ess)
(require-package 'lua-mode)

(require 'init-tex)
(require 'init-bib)

(require-package 'htmlize)
(require-package 'less-css-mode)

(require 'init-dired)
(require 'init-git)
(require 'init-projectile)
(require 'init-grep)

(require 'init-ido)
(require 'init-smex)

(require 'init-yas)

(require 'init-gnus)

(require 'init-untracked)

(server-start)
