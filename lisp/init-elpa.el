;; Modified from
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el.

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(provide 'init-elpa)
