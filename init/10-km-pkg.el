;; http://www.aaronbedra.com/emacs.d/

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'cl)
(defvar km/packages '(
                        evil
                        undo-tree
                        key-chord
                        org
                        ess
                        lua-mode
                        haskell-mode
                        markdown-mode
                        less-css-mode
                        auctex
                        magit
                        smex
                        paredit
                        )
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
