
(setq lispy-no-permanent-semantic t)

(add-hook 'emacs-lisp-mode-hook 'lispy-mode)
(add-hook 'emacs-lisp-mode-hook 'km/elisp-set-outline-vars)
;; This likely breaks lispy's outline functions, but I don't use them.
(add-hook 'lispy-mode-hook 'km/elisp-set-outline-vars)

(after 'lispy
  ;; This is ugly, but I haven't found another way to stop
  ;; `imenu-create-index-function' from being set to
  ;; `semantic-create-imenu-index'.  Trying to set it in
  ;; `emacs-lisp-mode-hook' or `lispy-mode-hook' doesn't work.
  (defalias 'semantic-create-imenu-index 'imenu-default-create-index-function))

(defun km/elisp-outline-level ()
  (and (looking-at (concat "^" outline-regexp))
       (- (match-end 0) (match-beginning 0) 3)))

(defun km/elisp-set-outline-vars ()
  (setq outline-regexp ";;;;* ")
  (setq outline-level 'km/elisp-outline-level))

(provide 'init-elisp)
