(require 'magit)

(defun km/magit-auto-commit ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
      (shell-command "git --no-pager commit --all --message=auto"))))

(define-key magit-status-mode-map (kbd "C-c C-u") 'km/magit-auto-commit)

;; http://whattheemacsd.com/setup-magit.el-05.html
(defun km/magit-just-amend ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

(define-key magit-status-mode-map (kbd "C-c C-a") 'km/magit-just-amend)
