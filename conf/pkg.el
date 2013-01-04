;; pkg.el
;; http://www.aaronbedra.com/emacs.d/

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'cl)
(defvar kyle-packages '(
                        evil
                        undo-tree
                        org
                        ess
                        lua-mode
			haskell-mode
                        )
  "Default packages")

(defun kyle-packages-installed-p ()
  (loop for pkg in kyle-packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (kyle-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg kyle-packages)
    (when (not (package-installed-p pkg))
      (message "installing %s" pkg)
      (package-install pkg))))
