(add-to-list 'load-path "~/src/emacs/git-modes")
(add-to-list 'load-path "~/src/emacs/magit")
(add-to-list 'load-path "~/src/emacs/orgit")

(require 'magit-autoloads)
(require 'git-modes-autoloads)

(require 'orgit)

(require-package 'git-annex)
(require 'git-annex)

(require-package 'git-timemachine)

(setq git-annex-commit nil)

(require 'magit-annex-autoloads)

(after 'magit-annex
  (setq magit-annex-all-action-arguments
        (delete "--auto" magit-annex-all-action-arguments)))

(key-chord-define-global ",g" 'magit-status)

(defun km/magit-auto-commit ()
  "Commit all changes with \"auto\" commit message.
This can be useful for non-source code repos (e.g., Org mode note
files) or commiting incomplete changes that will be extended into
a proper commit."
  (interactive)
  (magit-run-git "commit" "--all" "--message=auto"))

(defun km/magit-show-commit-under-point ()
  "Pass text at point as commit to `magit-show-commit'.
This is useful for commit IDs in files and log messages."
  (interactive)
  (--when-let (thing-at-point 'word)
    (magit-show-commit it)))

(defun km/magit-show-project-commit-under-point ()
  "Show commit under point for a selected project."
  (interactive)
  (--when-let (thing-at-point 'word)
    (let ((projectile-switch-project-action
           '(lambda () (magit-show-commit it))))
      (projectile-switch-project))))

(defun km/magit-commit-extend-all ()
  "Run `magit-commit-extend' with '--all' flag.
This can easily be done from the popup menu but is put into a
function for calling from outside Magit buffers."
  (interactive)
  (magit-commit-extend '("--all")))

(defun km/magit-stage-file-intent (file)
  "Stage FILE but not its content.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation.
\n(git add -N FILE)"
  ;; Modified from `magit-stage-file'.
  (interactive
   (let* ((atpoint (magit-section-when (file)))
          (current (magit-file-relative-name))
          (choices (magit-untracked-files))
          (default (car (member (or atpoint current) choices))))
     (list (if (or current-prefix-arg (not default))
               (magit-completing-read "Stage file" choices
                                      nil t nil nil default)
             default))))
  (magit-run-git "add" "-N" file))

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
\n(git checkout -t REMOTE-BRANCH\)"
  (interactive
   (list (magit-completing-read "Remote branch"
                                (magit-list-remote-branch-names))))
  (magit-run-git "checkout" "-t" remote-branch))

(defun km/magit-checkout-previous-branch ()
  "Checkout previous branch.
\n(git checkout -)"
  (interactive)
  (magit-run-git "checkout" "-"))

(defun km/magit-checkout-master ()
  "Checkout master branch.
\n(git checkout master)"
  (interactive)
  (magit-run-git "checkout" "master"))

(defun km/magit-branch-and-checkout-from-current (branch)
  "Create and checkout BRANCH at current branch.
This is equivalent to running `magit-branch-and-checkout' with
START-POINT set to the current branch.
\n(git checkout -b BRANCH)"
  (interactive (list (magit-read-string "Branch name")))
  (magit-run-git "checkout" "-b" branch))

(defun km/magit-backup-branch ()
  "Create a backup branch for the current branch.
\n(git branch b/<current-branch>)"
  (interactive)
  (--if-let (magit-get-current-branch)
      (magit-run-git "branch" (concat "b/" it))
    (user-error "No current branch")))

;; http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  ad-do-it
  (delete-other-windows))

(setq magit-restore-window-configuration t
      magit-completing-read-function 'magit-ido-completing-read
      magit-delete-by-moving-to-trash nil
      magit-popup-show-help-echo nil
      magit-popup-show-help-section nil
      magit-popup-use-prefix-argument 'default
      magit-log-show-margin nil)

(setq vc-follow-symlinks t)

(after 'git-commit-mode
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(after 'magit
  (magit-backup-mode -1)
  (add-hook 'magit-find-file-hook 'view-mode)

  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

  (define-key magit-popup-mode-map (kbd "SPC <t>") 'magit-invoke-popup-switch)
  (define-key magit-popup-mode-map (kbd "SPC SPC <t>") 'magit-invoke-popup-option)

  ;; Remove `magit-add-change-log-entry-other-window', which overrides
  ;; my binding for `km/zsh-ansi-term-other-window'.
  (define-key magit-mode-map (kbd "C-x 4 a") nil)

  (define-key magit-mode-map "N" 'km/magit-stage-file-intent)

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
  (magit-define-popup-action 'magit-branch-popup
    ?C "Create" 'magit-branch)
  (magit-define-popup-action 'magit-branch-popup
    ?c "Create & checkout from current"
    'km/magit-branch-and-checkout-from-current)
  (magit-define-popup-action 'magit-branch-popup
    ?p "Checkout previous" 'km/magit-checkout-previous-branch)
  (magit-define-popup-action 'magit-branch-popup
    ?m "Checkout master" 'km/magit-checkout-master)

  (defadvice magit-merge-editmsg (around km/magit-merge-editmsg-no-ff activate)
    "Set '--no-ff' flag when running `magit-merge-editmsg'."
    (let ((args '("--no-ff")))
      ad-do-it))

  (setq magit-branch-arguments
        (delete "--track" magit-branch-arguments)))

(define-key ctl-x-4-map "g" 'magit-find-file-other-window)
(define-key km/file-map "g" 'magit-find-file)

(define-prefix-command 'km/git-map)
(global-set-key (kbd "C-c g") 'km/git-map)
(define-key km/git-map "c" 'km/magit-show-commit-under-point)
(define-key km/git-map "C" 'km/magit-show-project-commit-under-point)
(define-key km/git-map "e" 'km/magit-commit-extend-all)
(define-key km/git-map "u" 'km/magit-auto-commit)

(provide 'init-git)
