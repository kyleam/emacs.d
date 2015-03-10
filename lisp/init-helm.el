
(require 'helm)
(require 'helm-config)

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-skip-boring-files t)

(defun km/helm-ff-org-open-file ()
  "Run `org-open-file' from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action #'org-open-file)))

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x c") nil)

(after 'helm-files
  (define-key helm-find-files-map (kbd "C-c x") 'km/helm-ff-org-open-file))

(define-key helm-map ";" 'helm-maybe-exit-minibuffer)

(key-chord-define-global "jc" 'helm-find-files)
(key-chord-define-global "jt" 'helm-mini)
(key-chord-define-global "kx" 'helm-M-x)

(define-key search-map "k" 'helm-swoop)

(helm-mode 1)

(provide 'init-helm)
