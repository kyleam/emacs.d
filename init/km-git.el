(require 'git-annex)

(setq git-annex-commit nil)

(require 'magit)

(defun km/magit-auto-commit ()
  "Commit all changes with \"auto\" commit message.
Useful for non-source code repos (e.g., Org mode note files)."
  (interactive)
  (magit-run-git "commit" "--all" "--message=auto"))

(magit-key-mode-insert-action 'committing
                              "u" "Auto commit" 'km/magit-auto-commit)

;; http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  ad-do-it
  (delete-other-windows))

(setq magit-restore-window-configuration t)

(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

(setq magit-expand-staged-on-commit t)
(setq magit-log-show-margin nil)
