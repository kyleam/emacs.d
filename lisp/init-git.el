(require 'git-annex)

(setq vc-follow-symlinks t)

(setq git-annex-commit nil)

(define-prefix-command 'km/git-map)
(global-set-key (kbd "C-c g") 'km/git-map)


;;; Magit

(add-to-list 'load-path "~/src/emacs/magit/lisp/")
(require 'magit)

(add-to-list 'load-path "~/src/emacs/orgit/")
(require 'orgit)

(setq magit-restore-window-configuration t
      magit-revert-buffers 'silent
      magit-push-always-verify nil
      magit-delete-by-moving-to-trash nil
      magit-diff-auto-show-delay 0.1
      magit-log-section-arguments nil
      magit-log-show-margin nil)

(setq git-commit-finish-query-functions nil)

(setq magit-no-confirm '(stage-all-changes unstage-all-changes reverse))

(setq git-commit-finish-query-functions nil)

(add-hook 'magit-find-file-hook 'view-mode)
;; http://whattheemacsd.com/setup-magit.el-01.html
(add-hook 'magit-status-mode-hook 'delete-other-windows)
(remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

(add-hook 'git-commit-setup-hook
          (lambda ()
            (add-hook 'with-editor-pre-finish-hook
                      'git-commit-save-message nil t)))
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(defun km/magit-auto-commit ()
  "Commit all changes with \"auto\" commit message.
This can be useful for non-source code repos (e.g., Org mode note
files) or commiting incomplete changes that will be extended into
a proper commit."
  (interactive)
  (magit-run-git "commit" "--all" "--message=auto"))

(defun km/magit-show-commit-at-point (&optional choose-project)
  "Show commit point.
If there is no current project or if the prefix argument
CHOOSE-PROJECT is non-nil, prompt for the project name."
  (interactive "P")
  (if (save-excursion (skip-chars-backward "A-z0-9")
                      (looking-at "\\b[A-z0-9]\\{4,40\\}\\b"))
      (let* ((hash (match-string-no-properties 0))
             (project
              (and (or choose-project
                       (not (projectile-project-p))
                       (not (magit-rev-verify (concat hash "^{commit}"))))
                   (completing-read "Project: "
                                    (projectile-relevant-known-projects))))
             (default-directory (or project default-directory)))
        (magit-show-commit hash))
    (user-error "No hash found at point")))

(defun km/magit-commit-extend-with-file ()
  "Extend last commit with changes in the current file."
  (interactive)
  (let ((file (or (buffer-file-name (buffer-base-buffer))
                  (user-error "Not visiting file"))))
    (cond ((magit-anything-staged-p)
           (user-error "There are already staged changes"))
          ((member (magit-file-relative-name file) (magit-modified-files))
           (magit-stage-file file)
           (magit-commit-extend))
          (t
           (message "No changes to %s" file)))))

(defun km/magit-commit-wip-with-file ()
  "Make a WIP commit for the current file.
Unlike `magit-wip-*' commands, this commit is made to the current
branch."
  (interactive)
  (let ((file (or (buffer-file-name (buffer-base-buffer))
                  (user-error "Not visiting file"))))
    (cond ((magit-anything-staged-p)
           (user-error "There are already staged changes"))
          ((member (magit-file-relative-name file) (magit-modified-files))
           (magit-stage-file file)
           (magit-run-git "commit" (format "--message=WIP %s"
                                           (magit-file-relative-name file))))
          (t
           (message "No changes to %s" file)))))

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
  (magit-run-git-async "push" "-v"
                       (magit-read-remote "Remote")
                       "--all"))

(defun km/magit-push-head (remote &optional args)
  "Push current branch to same name on remote.
\n(git push [ARGS] REMOTE HEAD)"
  (interactive (list (magit-read-remote "Remote") (magit-push-arguments)))
  (magit-run-git-async "push" "-v" args remote "HEAD"))

(defun km/magit-checkout-local-tracking (remote-branch)
  "Create and checkout a local tracking branch for REMOTE-BRANCH.
\n(git checkout -t REMOTE-BRANCH\)"
  (interactive
   (list (magit-completing-read "Remote branch"
                                (magit-list-remote-branch-names))))
  (magit-run-git "checkout" "-t" remote-branch))

(defun km/magit-delete-previous-branch (&optional force)
  "Delete previous branch.
\n(git branch -d @{-1})"
  (interactive "P")
  (magit-run-git "branch" (if force "-D" "-d") "@{-1}"))

(defun km/magit-checkout-previous-branch ()
  "Checkout previous branch.
\n(git checkout -)"
  (interactive)
  (magit-run-git "checkout" "-"))

(defun km/magit-list-recent-refs (n &optional remote)
  "List N recent refs.
If REMOTE is non-nil, limit to remote refs."
  (magit-git-lines
   "for-each-ref" "--sort=-committerdate" "--format=%(refname:short)"
   (format "--count=%s" n)
   (if remote "refs/remotes" "refs/heads")))

(defun km/magit-checkout-recent-ref (n)
  "Checkout branch from N recent refs.
Refs are sorted by committer date."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         5)))
  (magit-run-git "checkout"
                 (magit-completing-read
                  "Ref" (km/magit-list-recent-refs n))))

(defun km/magit-checkout-track-recent-ref (n)
  "Create and checkout a local tracking branch.
Listed refs are limited to N most recent, sorted by committer
date."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         5)))
  (magit-run-git "checkout" "-t"
                 (magit-completing-read
                  "Ref" (km/magit-list-recent-refs n 'remote))))

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

(defun km/magit-reset-file (rev file &optional checkout)
  "Reset FILE from revision REV.

If prefix argument CHECKOUT is non-nil, checkout FILE from REV
instead.

\(git reset REV -- FILE)
\(git checkout REV -- FILE)"
  (interactive
   (let ((rev (magit-read-branch-or-commit "Revision" magit-buffer-revision)))
     (list rev (magit-read-file-from-rev rev "File") current-prefix-arg)))
  (magit-with-toplevel
    (magit-run-git (if checkout "checkout" "reset")
                   rev "--" file)))

(defun km/magit-pin-file (&optional other-rev)
  "Pin this file to the current revision.

Visit the current file and current revision with
`magit-find-file'.  Position point as in the original buffer.
This may not correspond to same content if text before point has
changed since the current commit.

If OTHER-REV is non-nil, prompt for another revision instead of
the current.  If buffer is already a revision buffer, then find
the working tree copy instead.  In both these cases, point may
not land in a reasonable location depending on how the content of
the file has changed."
  (interactive "P")
  (magit-with-toplevel
    (let* ((line (+ (if (bolp) 1 0) (count-lines 1 (point))))
           (col (current-column))
           (rev (cond (other-rev
                       (magit-read-branch-or-commit "Find file from revision"))
                      ((not magit-buffer-file-name)
                       (or (magit-get-current-branch)
                           (magit-rev-parse "HEAD")))))
           (fname (file-relative-name
                   (or buffer-file-name
                       magit-buffer-file-name
                       (user-error "Buffer not visiting file")))))
      (if rev (magit-find-file rev fname) (find-file fname))
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col))))

(defun km/magit-revfile-reset (&optional checkout)
  "Reset to revision from current revfile.
If CHECKOUT is non-nil, checkout file instead."
  (interactive "P")
  (unless (and magit-buffer-refname magit-buffer-file-name)
    (user-error "Not in Magit revfile buffer"))
  (magit-with-toplevel
    (magit-run-git (if checkout "checkout" "reset")
                   magit-buffer-refname "--" magit-buffer-file-name)))

(defun km/magit-find-recently-changed-file (n)
  "Find a file that changed from \"HEAD~N..HEAD\".
N defaults to 20."
  (interactive "p")
  (unless current-prefix-arg (setq n 10))
  (magit-with-toplevel
    (find-file (magit-completing-read
                "File"
                (magit-changed-files (format "HEAD~%s..HEAD" n))
                nil t))))

(defun km/magit-find-commit-file (commit)
  "Find file changed in COMMIT."
  (interactive (list (or (magit-branch-or-commit-at-point)
                         (and (derived-mode-p 'magit-revision-mode)
                              (car magit-refresh-args))
                         (magit-read-branch-or-commit "Commit"))))
  (let ((files (magit-changed-files (format "%s~..%s" commit commit))))
    (find-file
     (cl-case (length files)
       (0 (user-error "No changed files in %s" commit))
       (1 (car files))
       (t (magit-completing-read "File" files nil t))))))

(defun km/magit-insert-staged-file (&optional no-directory)
  "Select staged file to insert.

This is useful for referring to file names in commit messages.
By default, the path for the file name is relative to the top
directory of the repository.  Remove the directory component from
the file name if NO-DIRECTORY is non-nil.

If there are no staged files, look instead at files that changed
in HEAD.  These rules will usually offer the files of interest
while commiting, but this is not the case if you are amending a
commit with the \"--only\" flag and have staged files (i.e., this
command will still offer the staged files)."
  (interactive "P")
  (magit-with-toplevel
    (let* ((files (or (magit-staged-files)
                      (magit-changed-files "HEAD^..HEAD")))
           (file (cl-case (length files)
                   (1 (car files))
                   (0 (error "No files found"))
                   (t
                    (completing-read "Staged file: " files nil t)))))
      (insert (if no-directory (file-name-nondirectory file) file)))))

(defun km/magit-shorten-hash (hash &optional n)
  (magit-rev-parse (format "--short=%s" (or n (magit-abbrev-length))) hash))

(defun km/magit-shorten-hash-at-point (&optional n)
  "Shorten hash at point to N characters.

N defaults to `magit-abbrev-length'.  If the commit belongs to
the current repo and the hash is ambiguous, the hash is extended
as needed.

To explicitly set the hash length, use a numeric prefix
argument."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         (magit-abbrev-length))))
  (cond
   ((< n 4)
    (user-error "Hash must be at least 4 characters"))
   ((>= n 40)
    (user-error "Full hashes are 40 characters"))
   ((> n 30)
    (message "That doesn't seem incredibly useful, but OK")))
  (let ((offset (- (skip-chars-backward "A-z0-9"))))
    (if (looking-at "\\b[A-z0-9]\\{5,40\\}\\b")
        (let ((hash-len (- (match-end 0) (match-beginning 0)))
              (hash (match-string 0)))
          (when (< hash-len n)
            (user-error "Desired hash length is greater than current"))
          (replace-match (km/magit-shorten-hash hash n)
                         'fixedcase)
          (when (< offset n)
            (skip-chars-backward "A-z0-9")
            (goto-char (+ (point) offset))))
      (goto-char (+ (point) offset))
      (user-error "No hash found at point"))))

(defvar km/magit-copy-hook
  '(km/magit-copy-commit-summary-from-header
    km/magit-copy-commit-message
    km/magit-copy-region-commits
    km/magit-copy-region-hunk
    km/magit-copy-hunk)
  "Functions tried by `km/magit-copy-as-kill'.
These will be given one argument (the current prefix value) and
should succeed by copying and returning non-nil or fail by
returning nil.")

(defun km/magit-copy-commit-summary (commit)
  "Copy a citation for the COMMIT at point.
Format the reference as '<hash>, (<subject>, <date>)'.  If there
is no commit at point or with a prefix argument, prompt for
COMMIT."
  (interactive
   (let ((atpoint (or (and magit-blame-mode (magit-blame-chunk-get :hash))
                      (magit-branch-or-commit-at-point)
                      (magit-tag-at-point))))
     (list (or (and (not current-prefix-arg) atpoint)
               (magit-read-branch-or-commit "Commit" atpoint)))))
  (if (magit-rev-verify (concat commit "^{commit}"))
      (kill-new (message
                 ;; Using `magit-git-string' instead of
                 ;; `magit-rev-format' to pass --date flag.
                 (magit-git-string "show" "-s" "--date=short"
                                   "--format=%h (%s, %ad)"
                                   commit "--")))
    (user-error "%s does not exist" commit)))

(defun km/magit-copy-commit-summary-from-header (&optional arg)
  (magit-section-when headers
    (km/magit-copy-commit-summary (car magit-refresh-args))))

(defun km/magit-copy-region-commits (&optional arg)
  (--when-let (magit-region-values 'commit)
    (deactivate-mark)
    (kill-new
     (mapconcat #'identity it
                (if arg (read-string "Separator: ") ", ")))))

(defun km/magit-copy-commit-message (&optional arg)
  (magit-section-when message
    (kill-new (replace-regexp-in-string
                    "^    " ""
                    (buffer-substring-no-properties (magit-section-start it)
                                                    (magit-section-end it))))))

(defun km/magit-copy-region-hunk (&optional no-column)
  (when (magit-section-internal-region-p)
    (magit-section-when hunk
      (deactivate-mark)
      (let ((text (buffer-substring-no-properties
                   (region-beginning) (region-end))))
        (kill-new (if no-column
                      (replace-regexp-in-string "^[ \\+\\-]" "" text)
                    text))))))

(defun km/magit-copy-hunk (&optional arg)
  (magit-section-when hunk
    (let ((start (save-excursion (goto-char (magit-section-start it))
                                 (1+ (point-at-eol)))))
      (kill-new (buffer-substring-no-properties
                 start (magit-section-end it))))))

(defun km/magit-copy-as-kill ()
  "Try `km/magit-copy-hook' before calling `magit-copy-as-kill'.
With a prefix argument of -1, always call `magit-copy-as-kill'.
Otherwise, the current prefix argument is passed to each hook
function."
  (interactive)
  (or (unless (= (prefix-numeric-value current-prefix-arg) -1)
        (run-hook-with-args-until-success
         'km/magit-copy-hook current-prefix-arg))
      (magit-copy-as-kill)))

(defun km/magit-avy-goto-subword-1 ()
  "Like `km/avy-goto-subword-1', but maybe show commit and limit to window."
  (interactive)
  (let (avy-all-windows)
    (call-interactively #'avy-goto-subword-1))
  (when (derived-mode-p 'magit-log-mode)
    (magit-diff-show-or-scroll-up)))

(defun km/magit-rev-ancestor-p (rev-a rev-b)
  "Report whether REV-A is the ancestor of REV-B.
Use the revision at point as REV-B.  With prefix argument or if
there is no revision at point, prompt for the revision.  Always
prompt for REV-A."
  (interactive
   (let* ((atpoint (or (and magit-blame-mode (magit-blame-chunk-get :hash))
                       (magit-branch-or-commit-at-point)
                       (magit-tag-at-point)))
          (commit (or (and (not current-prefix-arg) atpoint)
                      (magit-read-branch-or-commit "Descendant" atpoint))))
     (list (magit-read-other-branch-or-commit
            (format "Test if ancestor of %s" commit) commit)
           commit)))
  (message "%s is %san ancestor of %s" rev-a
           (if (magit-git-success "merge-base" "--is-ancestor"
                                  rev-a rev-b)
               "" "NOT ")
           rev-b))

(defun km/magit-refs-toggle-tags ()
  "Toggle showing tags in `magit-refs-mode'.
This only affects the current buffer and is useful if you do not
show tags by default."
  (interactive)
  (if (memq 'magit-insert-tags magit-refs-sections-hook)
      (remove-hook 'magit-refs-sections-hook 'magit-insert-tags t)
    (add-hook 'magit-refs-sections-hook 'magit-insert-tags t t))
  (magit-refresh-buffer))

(define-key ctl-x-4-map "g" 'magit-find-file-other-window)
(define-key km/file-map "g" 'magit-find-file)

(key-chord-define-global "jg" 'magit-status)

;; Remove `magit-add-change-log-entry-other-window', which overrides
;; my binding for `km/zsh-ansi-term-other-window'.
(define-key magit-mode-map (kbd "C-x 4 a") nil)
(define-key magit-mode-map "N" 'km/magit-stage-file-intent)
(define-key magit-mode-map "Q" 'km/magit-mode-bury-all-windows)
(define-key magit-mode-map (kbd "C-w") 'km/magit-copy-as-kill)

;; `magit-diff-visit-file-worktree' is also on C-RET.
(define-key magit-file-section-map (kbd "C-j") 'magit-diff-visit-file-worktree)
(define-key magit-hunk-section-map (kbd "C-j") 'magit-diff-visit-file-worktree)

(define-key magit-log-mode-map "j" 'km/magit-avy-goto-subword-1)
(define-key magit-refs-mode-map "j" 'km/magit-avy-goto-subword-1)
(define-key magit-cherry-mode-map "j" 'km/magit-avy-goto-subword-1)

(define-key magit-refs-mode-map (kbd "C-c C-t") 'km/magit-refs-toggle-tags)

(define-key magit-process-mode-map (kbd "C-c C-k") 'magit-process-kill)

(define-prefix-command 'km/magit-map)
(define-key magit-mode-map "." 'km/magit-map)
(define-key km/magit-map "c" 'km/magit-find-commit-file)
(define-key km/magit-map "g" 'km/git-map)

(define-prefix-command 'km/magit-wip-map)
(define-key km/git-map "w" 'km/magit-wip-map)
(define-key km/magit-wip-map "a" 'magit-wip-after-apply-mode)
(define-key km/magit-wip-map "b" 'magit-wip-before-change-mode)
(define-key km/magit-wip-map "c" 'magit-wip-commit)
(define-key km/magit-wip-map "f" 'magit-wip-commit-buffer-file)
(define-key km/magit-wip-map "l" 'magit-wip-log-current)
(define-key km/magit-wip-map "o" 'magit-wip-log)
(define-key km/magit-wip-map "s" 'magit-wip-after-save-mode)
(define-key km/magit-wip-map "S" 'magit-wip-after-save-local-mode)
(define-key km/magit-wip-map "w" 'km/magit-commit-wip-with-file)

(define-key km/git-map "." 'km/magit-show-commit-at-point)
(define-key km/git-map "c" 'km/magit-copy-commit-summary)
(define-key km/git-map "d" 'magit-dispatch-popup)
(define-key km/git-map "e" 'km/magit-commit-extend-with-file)
(define-key km/git-map "f" 'km/magit-reset-file)
(define-key km/git-map "n" 'km/magit-shorten-hash-at-point)
(define-key km/git-map "l" 'magit-log-buffer-file)
(define-key km/git-map "p" 'km/magit-pin-file)
(define-key km/git-map "r" 'km/magit-find-recently-changed-file)
(define-key km/git-map "s" 'km/magit-insert-staged-file)
(define-key km/git-map "u" 'km/magit-auto-commit)
(define-key km/git-map "v" 'km/magit-revfile-reset)

(define-key magit-log-select-mode-map "."
  'km/magit-log-select-guess-fixup-commit)


;;; Magit popups

(setq magit-popup-show-help-echo nil
      magit-popup-show-common-commands nil
      magit-popup-use-prefix-argument 'default)

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

(magit-define-popup-action 'magit-diff-popup
  ?e "Edit options" 'magit-diff-refresh-popup)

(magit-define-popup-action 'magit-stash-popup
  ?s "Snapshot" 'magit-snapshot)

(magit-define-popup-switch 'magit-log-popup
  ?p "First parent" "--first-parent")
(magit-define-popup-switch 'magit-log-popup
  ?t "Date order" "--date-order")

(magit-define-popup-action 'magit-log-popup
  ?e "Edit options" 'magit-log-refresh-popup)

(magit-define-popup-action 'magit-branch-popup
  ?c "Create & checkout from current"
  'km/magit-branch-and-checkout-from-current)
(magit-define-popup-action 'magit-branch-popup
  ?C "Create" 'magit-branch)
(magit-define-popup-action 'magit-branch-popup
  ?l "Delete previous branch"
  'km/magit-delete-previous-branch)
(magit-define-popup-action 'magit-branch-popup
  ?m "Checkout master" 'km/magit-checkout-master)
(magit-define-popup-action 'magit-branch-popup
  ?n "Checkout recent ref" 'km/magit-checkout-recent-ref)
(magit-define-popup-action 'magit-branch-popup
  ?N "Track recent ref" 'km/magit-checkout-track-recent-ref)
(magit-define-popup-action 'magit-branch-popup
  ?p "Checkout previous" 'km/magit-checkout-previous-branch)
(magit-define-popup-action 'magit-branch-popup
  ?s "Backup current branch" 'km/magit-backup-branch)
(magit-define-popup-action 'magit-branch-popup
  ?t "Local tracking" 'km/magit-checkout-local-tracking)

(defadvice magit-merge-editmsg (around km/magit-merge-editmsg-no-ff activate)
  "Set '--no-ff' flag when running `magit-merge-editmsg'."
  (let ((args '("--no-ff")))
    ad-do-it))


;;; Magit Annex

(add-to-list 'load-path "~/src/emacs/magit-annex/")
(require 'magit-annex)

(setq magit-annex-unused-open-function #'org-open-file)

(setq magit-annex-all-action-arguments
      (delete "--auto" magit-annex-all-action-arguments))


;;; Other git

(setq smerge-diff-switches '("-d" "-b" "-u"))

(define-key km/git-map "m"
  (defhydra hydra-smerge (:hint nil)
    "
_b_ keep base    _d_ diff     _n_ next
_m_ keep mine    _e_ ediff    _p_ previous
_o_ keep other   _h_ refine
_a_ keep all
\n"
    ("b" smerge-keep-base)
    ("m" smerge-keep-mine)
    ("o" smerge-keep-other)
    ("a" smerge-keep-all)
    ("n" smerge-next)
    ("p" smerge-prev)
    ("h" smerge-refine)
    ("e" smerge-ediff :color blue)
    ("d" (call-interactively
          (pcase (read-char-choice
                  "< base-mine, > base-other, = mine-other"
                  (list ?< ?> ?=))
            (?< #'smerge-diff-base-mine)
            (?> #'smerge-diff-base-other)
            (?= #'smerge-diff-mine-other))))
    ("l" recenter-top-bottom "recenter")
    ("u" undo "undo")
    ("q" nil "quit")))

(provide 'init-git)
