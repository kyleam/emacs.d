;; init.el

(message ":: Emacs init")
(add-to-list 'load-path "~/.emacs.d/conf")
(load "pkg.el")
(load "em.el")
(load "ev.el")
(load "om.el")
(load "ibuf.el")
(load "kb.el")
(load "tex.el")
(message ":: Init complete")
