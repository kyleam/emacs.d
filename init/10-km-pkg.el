;; http://www.aaronbedra.com/emacs.d/

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

(require 'cl)
(defvar km/packages '(ace-jump-mode
                      auctex
                      ess
                      git-commit-mode
                      haskell-mode
                      htmlize
                      key-chord
                      less-css-mode
                      lua-mode
                      magit
                      markdown-mode
                      org
                      paredit
                      smex
                      wrap-region
                      yasnippet)
  "Default packages")

(defun km/packages-installed-p ()
  (loop for pkg in km/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (km/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg km/packages)
    (when (not (package-installed-p pkg))
      (message "installing %s" pkg)
      (package-install pkg))))

(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'git-annex)
