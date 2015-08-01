
(add-hook 'emacs-lisp-mode-hook 'lispy-mode)
(add-hook 'emacs-lisp-mode-hook 'km/elisp-set-outline-vars)
;; This likely breaks lispy's outline functions, but I don't use them.
(add-hook 'lispy-mode-hook 'km/elisp-set-outline-vars)

(defun km/elisp-outline-level ()
  (and (looking-at (concat "^" outline-regexp))
       (- (match-end 0) (match-beginning 0) 3)))

(defun km/elisp-set-outline-vars ()
  (setq outline-regexp ";;;;* ")
  (setq outline-level 'km/elisp-outline-level))

(provide 'init-elisp)
