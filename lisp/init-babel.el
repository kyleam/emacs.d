(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (python . t)
   (R . t)
   (emacs-lisp . t)
   (latex . t)))

(provide 'init-babel)
