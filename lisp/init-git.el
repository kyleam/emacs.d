(add-to-list 'load-path "~/src/emacs/git-modes")
(add-to-list 'load-path "~/src/emacs/magit")
(require 'magit)

(require-package 'git-annex)
(require 'git-annex)

(setq git-annex-commit nil)

(add-hook 'magit-mode-hook 'turn-on-magit-annex)

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

(magit-define-popup-action 'magit-commit-popup
  ?u "Auto commit" 'km/magit-auto-commit)
(magit-define-popup-action 'magit-push-popup
  ?a "Push all" 'km/magit-push-all)

(add-to-list 'magit-log-popup-defaults "--all")

(diminish 'magit-auto-revert-mode)

;; http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  ad-do-it
  (delete-other-windows))

(setq magit-restore-window-configuration t
      magit-completing-read-function 'magit-ido-completing-read
      magit-popup-show-help-echo nil
      magit-popup-show-help-section nil
      magit-log-show-margin nil)

(setq vc-follow-symlinks t)

(provide 'init-git)
