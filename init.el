(server-start)

(mapc
 (lambda (f) (load-file f))
 (file-expand-wildcards "~/.emacs.d/init/*.el"))
