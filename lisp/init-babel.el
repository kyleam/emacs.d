;; Set up babel languages.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (python . t)
   (R . t)
   (emacs-lisp . t)
   (latex . t)))

;; Don't ask for confirmation before running code.
(setq org-confirm-babel-evaluate nil)

(provide 'init-babel)
