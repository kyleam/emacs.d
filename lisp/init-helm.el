
(require 'helm)
(require 'helm-config)

(setq helm-move-to-line-cycle-in-source t
      helm-ff-newfile-prompt-p nil
      helm-ff-file-name-history-use-recentf t
      helm-ff-skip-boring-files t)

(defun km/helm-display-buffer ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action #'display-buffer)))

(defun km/helm-display-file ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action
     (lambda (f)
       (display-buffer (find-file-noselect f))))))

(defun km/helm-ff-org-open-file ()
  "Run `org-open-file' from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action #'org-open-file)))

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x c") nil)

(after 'helm-files
  (define-key helm-find-files-map (kbd "C-c x") 'km/helm-ff-org-open-file)
  (define-key helm-generic-files-map (kbd "C-c x") 'km/helm-ff-org-open-file)
  ;; Overrides `helm-ff-run-switch-other-frame'.
  (define-key helm-find-files-map (kbd "C-c C-o") 'km/helm-display-file)
  (define-key helm-generic-files-map (kbd "C-c C-o") 'km/helm-display-file)
  ;; Overrides `helm-buffer-switch-other-frame'.
  (define-key helm-buffer-map (kbd "C-c C-o") 'km/helm-display-buffer))

(key-chord-define-global "jc" 'helm-find-files)
(key-chord-define-global "jt" 'helm-mini)
(key-chord-define-global "kx" 'helm-M-x)

(define-key search-map "k" 'helm-swoop)

(global-set-key (kbd "C-h a") 'helm-apropos)

(helm-mode 1)

(provide 'init-helm)
