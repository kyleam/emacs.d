(require-package 'magit)
(require-package 'git-commit-mode)
(require-package 'git-annex)
(require 'git-annex)

(setq git-annex-commit nil)

(key-chord-define-global ",g" 'magit-status)

(defun km/magit-auto-commit ()
  "Commit all changes with \"auto\" commit message.
Useful for non-source code repos (e.g., Org mode note files)."
  (interactive)
  (magit-run-git "commit" "--all" "--message=auto"))


(eval-after-load 'magit
  '(progn
     (magit-key-mode-insert-action 'committing
                                   "u" "Auto commit" 'km/magit-auto-commit)
     (diminish 'magit-auto-revert-mode)))

;; http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  ad-do-it
  (delete-other-windows))

(setq magit-restore-window-configuration t
      magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
      magit-completing-read-function 'magit-ido-completing-read
      magit-log-show-margin nil)


;; Don't use vc for git.
(setq vc-handled-backends
      (delete 'Git vc-handled-backends))

(provide 'init-git)
