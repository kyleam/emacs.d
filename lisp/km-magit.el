;;; km-magit.el --- Magit extensions

;; Copyright (C) 2012-2016 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/emacs.d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'avy)
(require 'git-rebase)
(require 'magit)
(require 'projectile)

(defun km/magit-auto-commit ()
  "Commit all changes with \"auto\" commit message.
This can be useful for non-source code repos (e.g., Org mode note
files) or commiting incomplete changes that will be extended into
a proper commit."
  (interactive)
  (magit-run-git "commit" "--all" "--message=auto"))

;;;###autoload
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
        (magit-show-commit hash (car (magit-diff-arguments))))
    (user-error "No hash found at point")))

;;;###autoload
(defun km/magit-commit-extend-with-file ()
  "Extend last commit with changes in the current file."
  (interactive)
  (let ((file (or (magit-current-file)
                  (user-error "No current file"))))
    (cond ((magit-anything-staged-p)
           (user-error "There are already staged changes"))
          ((member file (nconc (magit-untracked-files)
                               (magit-modified-files)))
           (magit-with-toplevel (magit-stage-file file))
           (magit-commit-extend))
          (t
           (message "No changes to %s" file)))))

;;;###autoload
(defun km/magit-commit-wip-with-file ()
  "Make a WIP commit for the current file.
Unlike `magit-wip-*' commands, this commit is made to the current
branch."
  (interactive)
  (let ((file (or (magit-current-file)
                  (user-error "No current file"))))
    (cond ((magit-anything-staged-p)
           (user-error "There are already staged changes"))
          ((member file (nconc (magit-untracked-files)
                               (magit-modified-files)))
           (magit-with-toplevel (magit-stage-file file))
           (magit-run-git "commit" (concat "--message=WIP " file)))
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
   (list (let ((branches (magit-list-remote-branch-names)))
           (magit-completing-read
            "Remote branch" branches nil t nil nil
            (car (member (magit-branch-or-commit-at-point)
                         branches))))))
  (magit-run-git "checkout" "-t" remote-branch))

(defun km/magit-branch-rename (old new &optional force)
  "Like `magit-branch-rename', but use old branch as initial prompt."
  (interactive
   (let ((branch (magit-read-local-branch "Rename branch")))
     (list branch
           (magit-read-string-ns (format "Rename branch '%s' to" branch)
                                 branch)
           current-prefix-arg)))
  (unless (string= old new)
    (magit-run-git-no-revert "branch" (if force "-M" "-m") old new)))

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

(defun km/magit-refs-filter-recent (n)
  "Limit branch list to N most recent.
Warning: I find this useful, but it's a hack that breaks
magit-section-backward and probably other things.  Hit `g` to
refresh the buffer, and all should be right again."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         5)))
  (unless (derived-mode-p 'magit-refs-mode)
    (user-error "Not in Magit Refs mode"))
  (let ((sec (magit-current-section))
        remote refs line-sec)
    (when (eq (magit-section-type sec) 'branch)
      (setq sec (magit-section-parent sec)))
    (when (eq (magit-section-type sec) 'remote)
      (setq remote (magit-section-value sec)))
    (setq refs
          (magit-git-lines
           "for-each-ref" "--sort=-committerdate" "--format=%(refname:short)"
           (format "--count=%s" n)
           (if remote (format "refs/remotes/%s" remote) "refs/heads")))
    (save-excursion
      (save-restriction
        (narrow-to-region (magit-section-content sec) (magit-section-end sec))
        (goto-char (point-min))
        (while (and (not (eobp))
                    (setq line-sec (magit-current-section))
                    (eq (magit-section-type line-sec) 'branch))
          (if (member (magit-section-value line-sec) refs)
              (forward-line 1)
            (let ((inhibit-read-only t))
              (delete-region (magit-section-start line-sec)
                             (magit-section-end line-sec)))))))))

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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun km/magit-revfile-reset (&optional checkout)
  "Reset to revision from current revfile.
If CHECKOUT is non-nil, checkout file instead."
  (interactive "P")
  (unless (and magit-buffer-refname magit-buffer-file-name)
    (user-error "Not in Magit revfile buffer"))
  (magit-with-toplevel
    (magit-run-git (if checkout "checkout" "reset")
                   magit-buffer-refname "--" magit-buffer-file-name)))

;;;###autoload
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

;;;###autoload
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
          (replace-match (or (km/magit-shorten-hash hash n)
                             ;; We're not in a repo.
                             (substring hash 0 n))
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

;;;###autoload
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
With a prefix argument of -1, always call `magit-copy-section-value'
Otherwise, the current prefix argument is passed to each hook
function."
  (interactive)
  (or (unless (= (prefix-numeric-value current-prefix-arg) -1)
        (run-hook-with-args-until-success
         'km/magit-copy-hook current-prefix-arg))
      (magit-copy-section-value)))

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

(defun km/magit-log-flip-revs ()
  "Swap revisions in log range."
  (interactive)
  (let ((range (caar magit-refresh-args)))
    (if (and range
             (derived-mode-p 'magit-log-mode)
             (string-match magit-range-re range))
        (progn
          (setf (caar magit-refresh-args)
                (concat (match-string 3 range)
                        (match-string 2 range)
                        (match-string 1 range)))
          (magit-refresh))
      (user-error "No range to swap"))))

(defun km/magit-flip-revs ()
  (interactive)
  (cond ((derived-mode-p 'magit-diff-mode)
         (call-interactively #'magit-diff-flip-revs))
        ((derived-mode-p 'magit-log-mode)
         (call-interactively #'km/magit-log-flip-revs))))

(defun km/magit-diff-visit-file (&optional prev-rev other-window)
  "Like `magit-diff-visit-file', but with the option to visit REV^.

If prefix argument PREV-REV is non-nil, visit file for REV^
instead of REV.  If not in `magit-revision-mode', the prefix
argument has no effect.

OTHER-WINDOW corresponds to `magit-diff-visit-file's OTHER-WINDOW
argument.  Interactively, this can be accessed using the command
`km/magit-diff-visit-file-other-window'."
  (interactive "P")
  (let ((magit-refresh-args (if (and prev-rev
                                     (derived-mode-p 'magit-revision-mode))
                                (cons (concat (car magit-refresh-args) "^")
                                      (cdr magit-refresh-args))
                              magit-refresh-args))
        (current-prefix-arg (and other-window (list 4))))
    (call-interactively #'magit-diff-visit-file)))

(defun km/magit-diff-visit-file-other-window (&optional prev-rev)
  (interactive "P")
  (km/magit-diff-visit-file prev-rev t))

(provide 'km-magit)
;;; km-magit.el ends here