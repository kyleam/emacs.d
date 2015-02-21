(require 'git-annex)

(setq vc-follow-symlinks t)

(setq git-annex-commit nil)

(define-prefix-command 'km/git-map)
(global-set-key (kbd "C-c g") 'km/git-map)


;;; Magit

(add-to-list 'load-path "~/src/emacs/magit/")
(require 'magit-autoloads)

(add-to-list 'load-path "~/src/emacs/orgit/")
(require 'orgit)

(setq magit-restore-window-configuration t
      magit-completing-read-function 'magit-ido-completing-read
      magit-delete-by-moving-to-trash nil
      magit-log-show-margin nil)

(add-hook 'magit-find-file-hook 'view-mode)
;; http://whattheemacsd.com/setup-magit.el-01.html
(add-hook 'magit-status-mode-hook 'delete-other-windows)
(after 'magit
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

  (magit-backup-mode -1))

(after 'git-commit
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

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
           (lambda () (magit-show-commit it))))
      (projectile-switch-project))))

(defun km/magit-commit-extend-all ()
  "Run `magit-commit-extend' with '--all' flag.
This can easily be done from the popup menu but is put into a
function for calling from outside Magit buffers."
  (interactive)
  (magit-commit-extend '("--all")))

(defun km/magit-ff-merge-upstream ()
  "Perform fast-forward merge of upstream branch.
\n(git merge --no-edit --ff-only <upstream>)"
  (interactive)
  (--if-let (magit-get-tracked-branch)
      (magit-merge it '("--ff-only"))
    (user-error "No upstream branch")))

(defun km/magit-stage-file-intent (file)
  "Stage FILE but not its content.
With a prefix argument or when there is no file at point, ask for
the file to be staged.  Otherwise, stage the file at point
without requiring confirmation.
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

(defun km/magit-push-head (remote &optional args)
  "Push current branch to same name on remote.
\n(git push [ARGS] REMOTE HEAD)"
  (interactive (list (magit-read-remote "Remote") (magit-push-arguments)))
  (magit-run-git-async "push" "-v" args remote "HEAD"))

(defun km/magit-log-all-branches (&optional args files)
  (interactive (magit-log-read-args nil t))
  (add-to-list 'args "--all")
  (magit-log-head args files))

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

(defun km/magit-mode-bury-all-windows (&optional kill-buffer)
  "Run `magit-mode-quit-window' until no longer in Magit buffer."
  (interactive "P")
  (while (derived-mode-p 'magit-mode)
    (magit-mode-bury-buffer kill-buffer)))

(defun km/magit-log-select-guess-fixup-commit (&optional ntop)
  "Guess commit from fixup/squash commmits.
Consider NTOP commits (default is 5) when searching for 'fixup!'
and 'squash!' titles."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         5)))
  (let (ntop-end msgs commit-pts)
    (save-excursion
      ;; Get limit for fixup/squash search.
      (goto-char (point-min))
      (setq ntop-end (line-end-position (1+ ntop)))
      ;; Get fixup and squash messages.
      (while (re-search-forward "[a-z0-9]+ \\(fixup!\\|squash!\\) \\(.+\\)"
                                ntop-end t)
        (push (match-string-no-properties 2) msgs))
      (when (not msgs)
        (user-error "No fixup or squash commits found"))
      ;; Find earliest commit.
      (goto-char (point-min))
      (dolist (msg msgs)
        (when (re-search-forward (concat "[a-z0-9]+ " msg "\n") nil t)
          (push (match-beginning 0) commit-pts))))
    (if commit-pts
        (goto-char (apply #'max commit-pts))
      (message "No matching commits found"))))

(defun km/magit-pin-file (&optional other-rev)
  "Pin this file to the current revision.

Visit the current file and current revision with
`magit-find-file'.  Position point as in the original buffer.
This may not correspond to same content if text before point has
changed since the current commit.

If OTHER-REV is non-nil, prompt for another revision instead of
the current.  In this case, keep point at the beginning of the
buffer."
  (interactive "P")
  (let ((pos (point))
        (rev (if other-rev
                 (magit-read-branch-or-commit "Find file from revision")
               (or (magit-get-current-branch)
                   (magit-rev-parse "HEAD"))))
        (fname (and (or buffer-file-name
                        (user-error "Buffer not visiting file"))
                    (file-relative-name buffer-file-name
                                        (magit-get-top-dir)))))
    (magit-find-file rev fname)
    (unless other-rev (goto-char pos))))

(defun km/git-rebase-show-commit ()
  "Show the commit on the current line if any.
Unlike `git-rebase-show-commit', display (but don't switch to)
the commit buffer. And no dinging."
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (--if-let (and (looking-at git-rebase-line)
                   (match-string 2))
        (magit-show-commit it t))))

(define-key ctl-x-4-map "g" 'magit-find-file-other-window)
(define-key km/file-map "g" 'magit-find-file)

(key-chord-define-global "jg" 'magit-status)

(after 'magit
  ;; Remove `magit-add-change-log-entry-other-window', which overrides
  ;; my binding for `km/zsh-ansi-term-other-window'.
  (define-key magit-mode-map (kbd "C-x 4 a") nil)
  (define-key magit-mode-map "N" 'km/magit-stage-file-intent)
  (define-key magit-mode-map "Q" 'km/magit-mode-bury-all-windows)

  ;; `magit-diff-visit-file-worktree' is also on C-RET.
  (define-key magit-file-section-map (kbd "C-j") 'magit-diff-visit-file-worktree)
  (define-key magit-hunk-section-map (kbd "C-j") 'magit-diff-visit-file-worktree)

  (define-key magit-log-mode-map "j" 'ace-jump-mode)
  (define-key magit-refs-mode-map "j" 'ace-jump-mode)
  (define-key magit-cherry-mode-map "j" 'ace-jump-mode)

  (define-key km/git-map "c" 'km/magit-show-commit-under-point)
  (define-key km/git-map "C" 'km/magit-show-project-commit-under-point)
  (define-key km/git-map "e" 'km/magit-commit-extend-all)
  (define-key km/git-map "p" 'km/magit-pin-file)
  (define-key km/git-map "u" 'km/magit-auto-commit))

(after 'magit-log
  (define-key magit-log-select-mode-map "."
    'km/magit-log-select-guess-fixup-commit))

(after 'git-rebase
  (define-key git-rebase-mode-map "\s" 'km/git-rebase-show-commit))


;;; Magit popups

(setq magit-popup-show-help-echo nil
      magit-popup-show-help-section nil
      magit-popup-use-prefix-argument 'default)

(after 'magit
  (setq magit-branch-arguments
        (delete "--track" magit-branch-arguments))

  (define-key magit-popup-mode-map (kbd "SPC <t>") 'magit-invoke-popup-switch)
  (define-key magit-popup-mode-map (kbd "SPC SPC <t>") 'magit-invoke-popup-option)

  (setq magit-patch-popup
        (plist-put magit-patch-popup :use-prefix 'popup))

  (magit-define-popup-action 'magit-commit-popup
    ?u "Auto commit" 'km/magit-auto-commit)
  (magit-define-popup-action 'magit-push-popup
    ?a "Push all" 'km/magit-push-all)
  (magit-define-popup-action 'magit-push-popup
    ?h "Push HEAD" 'km/magit-push-head)
  (magit-define-popup-action 'magit-merge-popup
    ?u "Merge upstream" 'km/magit-ff-merge-upstream)
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
      ad-do-it)))


;;; Magit Annex

(add-to-list 'load-path "~/src/emacs/magit-annex/")
(require 'magit-annex-autoloads)

(after 'magit-annex
  (setq magit-annex-all-action-arguments
        (delete "--auto" magit-annex-all-action-arguments)))

(provide 'init-git)
