(add-to-list 'load-path "~/src/emacs/git-modes")
(add-to-list 'load-path "~/src/emacs/magit")

(require 'magit-autoloads)
(require 'git-modes-autoloads)

(require-package 'git-annex)
(require 'git-annex)

(require-package 'git-timemachine)

(setq git-annex-commit nil)

(require 'magit-annex-autoloads)

(key-chord-define-global ",g" 'magit-status)

(defun km/magit-auto-commit ()
  "Commit all changes with \"auto\" commit message.
Useful for non-source code repos (e.g., Org mode note files)."
  (interactive)
  (magit-run-git "commit" "--all" "--message=auto"))

(defun km/magit-push-all ()
  "Push all branches."
  (interactive)
  (let ((remote (magit-read-remote "Remote")))
    (magit-run-git-async "push" "-v" remote "--all")))

(defun km/magit-log-all-branches (range &optional args)
  (interactive (magit-log-read-args t nil))
  (add-to-list 'args "--all")
  (magit-log-dwim range args))

(defun km/magit-checkout-local-tracking (remote-branch)
  "Create and checkout a local tracking branch for REMOTE-BRANCH.
\(git checkout -t REMOTE-BRANCH\)"
  (interactive
   (list (magit-completing-read "Remote branch"
                                (magit-list-remote-branch-names))))
  (magit-run-git "checkout" "-t" remote-branch))

(defun km/magit-backup-branch ()
  "Create a backup branch for the current branch.
\(git branch b/<current-branch>\)"
  (interactive)
  (-if-let (current-branch (magit-get-current-branch))
      (magit-run-git "branch" (concat "b/" current-branch))
    (user-error "No current branch")))

;; http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  ad-do-it
  (delete-other-windows))

(setq magit-restore-window-configuration t
      magit-completing-read-function 'magit-ido-completing-read
      magit-popup-show-help-echo nil
      magit-popup-show-help-section nil
      magit-find-file-hook 'view-mode
      magit-log-show-margin nil)

(setq vc-follow-symlinks t)

(after 'magit
  (define-key magit-popup-mode-map (kbd "SPC <t>") 'magit-invoke-popup-switch)
  (define-key magit-popup-mode-map (kbd "SPC SPC <t>") 'magit-invoke-popup-option)

  (define-key magit-mode-map (kbd "C-c m") 'km/magit-prefix-map)

  (magit-define-popup-action 'magit-commit-popup
                             ?u "Auto commit" 'km/magit-auto-commit)
  (magit-define-popup-action 'magit-push-popup
                             ?a "Push all" 'km/magit-push-all)
  (magit-define-popup-action 'magit-log-popup
                             ?a "All branches" 'km/magit-log-all-branches)
  (magit-define-popup-action 'magit-branch-popup
                             ?t "Local tracking" 'km/magit-checkout-local-tracking)
  (magit-define-popup-action 'magit-branch-popup
                             ?s "Backup current branch" 'km/magit-backup-branch)

  (setq magit-branch-popup-defaults
        (delete "--track" magit-branch-popup-defaults)))

(define-key ctl-x-4-map "g" 'magit-find-file-other-window)
(define-key km/file-map "g" 'magit-find-file)

(provide 'init-git)
