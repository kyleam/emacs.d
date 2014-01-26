(require-package 'magit)
(require-package 'git-commit-mode)
(require-package 'git-annex)

(setq git-annex-commit nil)

(key-chord-define-global ",g" 'magit-status)

(defun km/magit-auto-commit ()
  "Commit all changes with \"auto\" commit message.
Useful for non-source code repos (e.g., Org mode note files)."
  (interactive)
  (magit-run-git "commit" "--all" "--message=auto"))


(eval-after-load 'magit
    '(magit-key-mode-insert-action 'committing
                                   "u" "Auto commit" 'km/magit-auto-commit))

;; http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  ad-do-it
  (delete-other-windows))

(setq magit-restore-window-configuration t
      magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
      magit-log-show-margin nil)

(provide 'init-git)
