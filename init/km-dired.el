(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-load-hook
          (lambda () (load "dired-x")))
