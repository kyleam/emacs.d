;; init.el

(message ":: Emacs init")

(server-start)
(add-to-list 'load-path "~/.emacs.d/conf")

(defvar km/config-files '(
                          "km-pkg.el"
                          "km-ui.el"
                          "km-general.el"
                          "km-editor.el"
                          "km-func.el"
                          "km-org.el"
                          "km-evil.el"
                          "km-ibuffer.el"
                          "km-ido.el"
                          "km-mail.el"
                          "km-python.el"
                          "km-haskell.el"
                          "km-tex.el"
                          )
  "configuration files")

(defun km/load-config-files ()
  (dolist (cfile km/config-files)
        (load cfile)))

(defun km/load-config-files2 ()
  (dolist (cfile km/config-files)
        (message "loading %s" cfile)
        (load cfile)))

(km/load-config-files)

(message ":: Init complete")
