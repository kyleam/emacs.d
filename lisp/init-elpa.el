;; Modified from
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el.

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(package-initialize)

(provide 'init-elpa)
