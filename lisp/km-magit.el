;;; km-magit.el --- Magit extensions

;; Copyright (C) 2012-2018 Kyle Meyer <kyle@kyleam.com>

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
(require 'km-util)
(require 'magit)
(require 'projectile)

(defun km/magit-status (&optional default-display)
  "Like `magit-status', but change the meaning of the prefix argument.
If DEFAULT-DISPLAY is non-nil, use
`magit-display-buffer-traditional' as the value for
`magit-display-buffer-function'."
  (interactive "P")
  (let ((magit-display-buffer-function (if default-display
                                           #'magit-display-buffer-traditional
                                         magit-display-buffer-function))
        (current-prefix-arg nil))
    (call-interactively #'magit-status)))

(defun km/magit-auto-commit ()
  "Commit all changes with \"auto\" commit message.
This can be useful for non-source code repos (e.g., Org mode note
files) or commiting incomplete changes that will be extended into
a proper commit."
  (interactive)
  (magit-run-git "commit" "--all" "--message=auto"))

(defun km/magit-update-or-auto-commit (&optional no-directory)
  (interactive "P")
  (let ((files (delete-dups (nconc (magit-unstaged-files)
                                   (magit-staged-files)))))
    (cl-case (length files)
      (0 (user-error "No tracked files with changes"))
      (1
       (magit-run-git "commit" "--all" "--message"
                      (concat (funcall (if no-directory
                                           #'file-name-nondirectory
                                         #'identity)
                                       (car files))
                              ": Update")))
      (t
       (km/magit-auto-commit)))))

;;;###autoload
(defun km/magit-show-commit-at-point (&optional choose-project)
  "Show the commit at point.
Prompt for the project name in any of these cases: 1) the prefix
argument CHOOSE-PROJECT is non-nil, 2) there is no current
project, or 3) an commit object for the hash at point doesn't
exist in the current project."
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
  "Add the changes in the current file to the last commit."
  (interactive)
  (let ((file (or (magit-current-file)
                  (user-error "No current file"))))
    (cond ((magit-anything-staged-p)
           (user-error "There are already staged changes"))
          ((member file (nconc (magit-untracked-files)
                               (magit-unstaged-files)))
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
                               (magit-unstaged-files)))
           (magit-with-toplevel (magit-stage-file file))
           (magit-run-git "commit" (concat "--message=WIP " file)))
          (t
           (message "No changes to %s" file)))))

(defun km/magit-ff-merge-upstream ()
  "Perform a fast-forward merge of the upstream branch.
\n(git merge --no-edit --ff-only <upstream>)"
  (interactive)
  (--if-let (magit-get-upstream-branch)
      (magit-merge it '("--ff-only"))
    (user-error "No upstream branch")))

;;;###autoload
(defun km/magit-merge-pull-message (rev)
  "Generate a PR merge message for REV.

The PR message can take two forms:

  1) Merge branch '<local>' [#<pr>]
  2) Merge pull request #<pr> from <remote>/<branch>

The first is used if REV is the name of a local branch, and the
second if REV is the name of a remote branch.

This assumes that you are pulling PRs into your 'refs/pull/'
namespace."
  (-when-let (pr (--when-let (magit-rev-name rev "refs/pull/*")
                   (and (string-match "\\`pull/.+/\\([0-9]+\\)\\'" it)
                        (match-string 1 it))))
    (cond ((magit-local-branch-p rev)
           (format "Merge branch '%s' [#%s]" rev pr))
          ((magit-remote-branch-p rev)
           (format "Merge pull request #%s from %s" pr rev)))))

(defun km/magit-push-all ()
  "Push all branches."
  (interactive)
  (magit-run-git-async "push" "-v"
                       (magit-read-remote "Remote")
                       "--all"))

(defun km/magit-push-head (remote &optional args)
  "Push the current branch to same name on REMOTE.
\n(git push [ARGS] REMOTE HEAD)"
  (interactive (list (magit-read-remote "Remote") (magit-push-arguments)))
  (magit-run-git-async "push" "-v" args remote "HEAD"))

(defun km/magit-checkout-local-tracking (remote-branch)
  "Create and check out a local tracking branch for REMOTE-BRANCH.
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
    (magit-run-git "branch" (if force "-M" "-m") old new)))

(defun km/magit-delete-previous-branch (&optional force)
  "Delete the previous branch.
\n(git branch -d @{-1})"
  (interactive "P")
  (magit-run-git "branch" (if force "-D" "-d") "@{-1}"))

(defun km/magit-checkout-previous-branch ()
  "Check out the previous branch.
\n(git checkout -)"
  (interactive)
  (magit-run-git "checkout" "-"))

(defun km/magit-list-recent-refs (n &optional remote)
  "List the N-most recent refs.
If REMOTE is non-nil, limit the results to remote refs."
  (magit-git-lines
   "for-each-ref" "--sort=-committerdate" "--format=%(refname:short)"
   (format "--count=%s" n)
   (if remote "refs/remotes" "refs/heads")))

(defun km/magit-checkout-recent-ref (n)
  "Check out branch from N-most recent refs.
Refs are sorted by committer date."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         5)))
  (magit-run-git "checkout"
                 (magit-completing-read
                  "Ref" (km/magit-list-recent-refs n))))

(defun km/magit-checkout-track-recent-ref (n)
  "Create and check out a local tracking branch.
Listed refs are limited to th eN-most recent, sorted by committer
date."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         5)))
  (magit-run-git "checkout" "-t"
                 (magit-completing-read
                  "Ref" (km/magit-list-recent-refs n 'remote))))

(defun km/magit-checkout-master ()
  "Check out master branch.
\n(git checkout master)"
  (interactive)
  (magit-run-git "checkout" "master"))

(defun km/magit-branch-and-checkout-from-current (branch)
  "Create and check out BRANCH at the current branch.
This is equivalent to running `magit-branch-and-checkout' with
START-POINT set to the current branch.
\n(git checkout -b BRANCH)"
  (interactive (list (magit-read-string "Branch name" nil nil
                                        (--when-let (magit-get-current-branch)
                                          (concat it "-tmp")))))
  (magit-run-git "checkout" "-b" branch))

(defun km/magit-branch-backup-current (&optional choose-name)
  "Create a backup branch for the current branch.
With the prefix argument CHOOSE-NAME, prompt for the name of the
backup branch.  Otherwise, name it as 'b/<current-branch>__v<n>',
where <n> is incremented to form a refname that doesn't already
exist."
  (interactive "P")
  (let ((current (or (magit-get-current-branch)
                     (user-error "No current branch"))))
    (magit-run-git
     "branch"
     (if choose-name
         (magit-read-string-ns "Backup name" current)
       (let* ((version-re (format "\\`%s__v\\([[:digit:]]+\\)\\'" current))
              (versions (delq nil
                              (mapcar
                               (lambda (s)
                                 (and (string-match version-re s)
                                      (string-to-number
                                       (match-string-no-properties 1 s))))
                               (magit-list-local-branch-names)))))
         (format "%s__v%d"
                 current
                 (if (null versions) 1 (1+ (apply #'max versions)))))))))

(defun km/magit-branch-archive (branches)
  "Move BRANCHES from refs/heads/ to refs/archive/."
  (interactive
   (list (or (magit-region-values 'branch)
             (list
              (magit-completing-read
               "Branch to archive" (magit-list-refnames "refs/heads")
               nil 'require nil nil
               (or (magit-branch-at-point) (magit-get-previous-branch)))))))
  (setq branches
        (mapcar (lambda (branch)
                  (cons
                   (replace-regexp-in-string "refs/heads/" "" branch)
                   (concat (and (not (string-prefix-p "refs/heads/" branch))
                                "refs/heads/")
                           branch)))
                branches))
  (pcase-dolist (`(,branch-short . ,branch-full) branches)
    (if (magit-git-success "update-ref"
                           (replace-regexp-in-string "refs/heads/"
                                                     "refs/archive/"
                                                     branch-full)
                           branch-full)
        (magit-run-git "branch" "-D" branch-short)
      (error "update-ref call failed")))
  (message (concat "Archived "
                   (let ((num-branches (length branches)))
                     (if (= num-branches 1)
                         (caar branches)
                       (format "%d branches" num-branches))))))

(defun km/magit-mode-bury-all-windows (&optional kill-buffer)
  "Run `magit-mode-quit-window' until no longer in Magit buffer."
  (interactive "P")
  (while (derived-mode-p 'magit-mode)
    (magit-mode-bury-buffer kill-buffer)))

(defun km/magit-log-select-guess-fixup-commit (&optional ntop)
  "Guess a commit based on fixup/squash commmits.
Consider NTOP commits (default is 5) when searching for 'fixup!'
and 'squash!' titles."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         5)))
  (let ((msg-fn (lambda ()
                  (magit-rev-format
                   "%s" (oref (magit-current-section) value))))
        msgs commit-pts)
    (save-excursion
      (goto-char (point-min))
      ;; Get fixup and squash messages.
      (while (re-search-forward (rx " " (or "fixup" "squash") "! ")
                                (line-end-position (1+ ntop))
                                t)
        (let ((msg (funcall msg-fn)))
          (and msg
               (string-match (rx string-start (or "fixup" "squash") "! "
                                 (group (one-or-more not-newline)))
                             msg)
               (push (cons (match-string-no-properties 1 msg) (line-end-position))
                     msgs))))
      (when (not msgs)
        (user-error "No fixup or squash commits found"))
      ;; Find earliest commit.
      (pcase-dolist (`(,msg . ,search-beg) msgs)
        (goto-char search-beg)
        (catch 'found
          (while (search-forward msg nil t)
            (when (string= msg (funcall msg-fn))
              (push (line-beginning-position) commit-pts)
              (throw 'found t))))))
    (if commit-pts
        (goto-char (apply #'max commit-pts))
      (message "No matching commits found"))))

;;;###autoload
(defun km/magit-reset-file (rev file &optional checkout)
  "Reset FILE from revision REV.

If prefix argument CHECKOUT is non-nil, check out FILE from REV
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
                   (or (buffer-file-name (buffer-base-buffer))
                       magit-buffer-file-name
                       (user-error "Buffer not visiting file")))))
      (if rev (magit-find-file rev fname) (find-file fname))
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col))))

;;;###autoload
(defun km/magit-revfile-reset (&optional checkout)
  "Reset to revision from current revfile.
If CHECKOUT is non-nil, check out file instead."
  (interactive "P")
  (unless (and magit-buffer-refname magit-buffer-file-name)
    (user-error "Not in Magit revfile buffer"))
  (magit-with-toplevel
    (magit-run-git (if checkout "checkout" "reset")
                   magit-buffer-refname "--" magit-buffer-file-name)))

;;;###autoload
(defun km/magit-find-recently-changed-file (&optional n)
  "Find a file that changed from \"HEAD~N..HEAD\".
N defaults to 20."
  (interactive (list (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (setq n (or n 10))
  (magit-with-toplevel
    (find-file (magit-completing-read
                "File"
                (magit-changed-files (format "HEAD~%s..HEAD" n))
                nil t))))

(defun km/magit-find-commit-file (commit)
  "Find a file that changed in COMMIT."
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
  "Select a staged file to insert.

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
  "Shorten the hash at point to N characters.

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

(defun km/magit-describe (rev)
  "Run 'git describe' on REV."
  (interactive
   (list (or (-when-let (section (magit-current-section))
               (cond
                ((memq (oref section type) '(commit branch))
                 (oref section value))
                ((derived-mode-p 'magit-revision-mode)
                 (car magit-refresh-args))))
             (magit-read-branch-or-commit "Revision"))))
  (--when-let (and rev (magit-git-string "describe" rev))
    (kill-new (message it))))

(defun km/magit-rev-ancestor-p (rev-a rev-b)
  "Report whether REV-A is the ancestor of REV-B."
  (interactive
   (let* ((rev-a (magit-read-branch-or-commit "Ancestor candidate")))
     (list rev-a (magit-read-other-branch-or-commit
                  (format "Is %s the ancestor of" rev-a)
                  rev-a))))
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

(defun km/magit-revision-insert-related-refs ()
  (interactive)
  (let ((magit-revision-insert-related-refs t))
    (magit-refresh)))

(defun km/magit-log-flip-revs ()
  "Swap the two revisions in a log's range."
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

(defun km/magit-cherry-flip-revs ()
  (interactive)
  (pcase-let ((`(,rev0 ,rev1) magit-refresh-args))
    (setf (nth 0 magit-refresh-args) rev1)
    (setf (nth 1 magit-refresh-args) rev0)
    (magit-refresh)))

(defun km/magit-flip-revs ()
  (interactive)
  (cond ((derived-mode-p 'magit-diff-mode)
         (call-interactively #'magit-diff-flip-revs))
        ((derived-mode-p 'magit-log-mode)
         (call-interactively #'km/magit-log-flip-revs))
        ((derived-mode-p 'magit-cherry-mode)
         (call-interactively #'km/magit-cherry-flip-revs))))

(defun km/magit-log-modify-range ()
  "Change the range for the current log buffer."
  (interactive)
  (unless (derived-mode-p 'magit-log-mode)
    (user-error "Not in log buffer"))
  (setf (caar magit-refresh-args)
        (read-string "Range: " (caar magit-refresh-args)))
  (magit-refresh))

(defun km/magit-cherry-insert-in-upstream ()
  (insert ?\n)
  (magit-insert-section (cherries)
    (magit-insert-heading "In upstream:")
    (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
      "cherry" "-v" "--abbrev"
      (nth 1 magit-refresh-args) (nth 0 magit-refresh-args))))

(defun km/magit-cherry-toggle-upstream-section ()
  (interactive)
  (let ((pos (point)))
    (if (memq #'km/magit-cherry-insert-in-upstream magit-cherry-sections-hook)
        (kill-local-variable 'magit-cherry-sections-hook)
      (setq-local magit-cherry-sections-hook
                  (append magit-cherry-sections-hook
                          '(km/magit-cherry-insert-in-upstream))))
    (magit-refresh-buffer)
    (goto-char pos)))

(defun km/magit-diff-visit-file (&optional prev-rev other-window)
  "Like `magit-diff-visit-file', but with the option to visit REV^.

If prefix argument PREV-REV is non-nil, visit the file for REV^
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

(defun km/magit-stash-edit-message (stash message)
  "Change STASH's message to MESSAGE."
  (interactive
   (let* ((stash (magit-read-stash "Rename" t))
          (old-msg (magit-git-string "show" "-s" "--format=%s" stash)))
     (list stash (magit-read-string "Stash message" old-msg))))
  (let ((commit (magit-rev-parse stash))
        (inhibit-magit-refresh t))
    (magit-stash-drop stash)
    (magit-stash-store message "refs/stash" commit))
  (magit-refresh))

(defun km/git-commit-turn-on-flyspell ()
  "Like `git-commit-turn-on-flyspell', but don't check buffer initially."
  (setq flyspell-generic-check-word-predicate
        'git-commit-flyspell-verify)
  (turn-on-flyspell))

(defun km/magit-log-dwim (&optional args files)
  (interactive (magit-log-arguments))
  (let ((range
         (or (magit-section-case
               ((unpushed unpulled)
                (oref it value))
               (tag
                (concat (oref it value) ".."))
               (branch
                (let ((current (magit-get-current-branch))
                      (atpoint (oref it value))
                      (upstream (magit-get-upstream-branch))
                      (push (magit-get-push-branch)))
                  (cond ((equal atpoint current)
                         (and upstream (concat upstream "..")))
                        ((equal atpoint push)
                         (concat push ".."))
                        (t
                         (concat ".." atpoint))))))
             (--when-let (and (derived-mode-p 'magit-revision-mode)
                              (car magit-refresh-args))
               (and (magit-rev-verify (concat it "^2"))
                    (concat it "^-1"))))))
    (if range
        (magit-log (list range) args files)
      (call-interactively #'magit-log))))

(defun km/magit-cherry-dwim ()
  (interactive)
  (-let [(head . upstream)
         (if (eq major-mode 'magit-log-mode)
             (let ((range (caar magit-refresh-args)))
               (and range
                    (string-match magit-range-re range)
                    (cons (match-string 3 range)
                          (match-string 1 range))))
           (let ((section (magit-current-section))
                 (current-branch (magit-get-current-branch)))
             (pcase (list (oref section type)
                          (oref section value))
               (`(unpushed "@{upstream}..")
                (cons current-branch (magit-get-upstream-branch)))
               (`(unpulled "..@{upstream}")
                (cons (magit-get-upstream-branch) current-branch))
               ;; Don't try to match "@{push}" because
               ;; `magit-insert-unpulled-from-pushremote' and
               ;; `magit-insert-unpulled-from-pushremote' avoid it to
               ;; be compatible with all push.default settings.
               (`(unpushed ,_)
                (cons current-branch (magit-get-push-branch)))
               (`(unpulled ,_)
                (cons (magit-get-push-branch) current-branch)))))]
    (if (and head upstream)
        (magit-cherry head upstream)
      (call-interactively #'magit-cherry))))

(defun km/magit--insert-count-lines (rev counts)
  (-let [(n-behind n-ahead) counts]
    (when (> n-ahead 0)
      (magit-insert-section (unpushed (concat rev ".."))
        (magit-insert-heading
          (format "%3s ahead of %s"
                  (propertize (number-to-string n-ahead)
                              'face 'magit-diffstat-added)
                  rev))))
    (when (> n-behind 0)
      (magit-insert-section (unpulled (concat ".." rev))
        (magit-insert-heading
          (format "%3s behind   %s"
                  (propertize (number-to-string n-behind)
                              'face 'magit-diffstat-removed)
                  rev))))))

(defun km/magit-insert-remote-counts ()
  "Insert a section showing number of unpushed and unpulled commits.

This function is a lightweight replacement of four
`magit-status-sections-hook' functions:
`magit-insert-unpulled-from-upstream',
`magit-insert-unpulled-from-pushremote',
`magit-insert-unpushed-to-upstream', and
`magit-insert-unpushed-to-pushremote'.

Unlike the above functions, this function does not insert a log
of unpulled or unpushed commits, but instead inserts a combined
section that only reports the number of commits in each
category."
  (when (magit-get-current-branch)
    (let* ((up-counts (and (magit-git-success "rev-parse" "@{upstream}")
                           (magit-rev-diff-count "@{upstream}" "")))
           ;; Unlike `magit-insert-unpushed-to-pushremote' and
           ;; `magit-insert-unpulled-from-pushremote', just use
           ;; "@{push}".  This drops support for the push-remote if
           ;; push.default isn't set to a compatible value.  See
           ;; Magit's 6505f4cd.
           (pu-counts (and (magit-git-success "rev-parse" "@{push}")
                           (magit-rev-diff-count "@{push}" "")))
           (up-any (and up-counts (or (> (car up-counts) 0)
                                      (> (cadr up-counts) 0))))
           (pu-any (and pu-counts (or (> (car pu-counts) 0)
                                      (> (cadr pu-counts) 0)))))
      (when (or up-any pu-any)
        (magit-insert-section (remote-counts)
          (magit-insert-heading "Remote counts")
          (when up-any
            (km/magit--insert-count-lines "@{upstream}" up-counts))
          (when (and pu-any (not (and up-any
                                      (equal (magit-rev-name "@{upstream}")
                                             (magit-rev-name "@{push}")))))
            (km/magit--insert-count-lines "@{push}" pu-counts)))))))

(magit-define-section-jumper magit-jump-to-remote-counts
  "Remote counts" remote-counts)


;;; Copy functions

(defvar km/magit-copy-functions
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
                 "%s"
                 ;; Using `magit-git-string' instead of
                 ;; `magit-rev-format' to pass --date flag.
                 (magit-git-string "show" "-s" "--date=short"
                                   "--format=%h (%s, %ad)"
                                   commit "--")))
    (user-error "%s does not exist" commit)))

(defun km/magit-copy-commit-summary-from-header (&optional _)
  (magit-section-when headers
    (km/magit-copy-commit-summary (car magit-refresh-args))))

(defun km/magit-copy-region-commits (&optional read-separator)
  (--when-let (magit-region-values 'commit)
    (deactivate-mark)
    (kill-new
     (message
      "%s"
      (mapconcat #'identity
                 it
                 (if read-separator (read-string "Separator: ") ", "))))))

(defun km/magit-copy--truncated-message (msg)
  (let ((msg-lines (split-string msg "[\n\r]+" 'omit-nulls)))
    (message "%s" (cl-case (length msg-lines)
                    (0 msg)
                    (1 (car msg-lines))
                    (t (concat (car msg-lines) "[...]"))))))

(defun km/magit-copy-commit-message (&optional _)
  (magit-section-when message
    (let ((msg (buffer-substring-no-properties (oref it start)
                                               (oref it end))))
      (kill-new msg)
      (km/magit-copy--truncated-message msg))))

(defun km/magit-copy-region-hunk (&optional no-column)
  (when (magit-section-internal-region-p)
    (magit-section-when hunk
      (deactivate-mark)
      (let ((text (buffer-substring-no-properties
                   (region-beginning) (region-end))))
        (setq text (if no-column
                       (replace-regexp-in-string "^[ \\+\\-]" "" text)
                     text))
        (kill-new text)
        (km/magit-copy--truncated-message text)))))

(defun km/magit-copy-hunk (&optional _)
  (magit-section-when hunk
    (kill-new (buffer-substring-no-properties
               (save-excursion (goto-char (oref it start))
                               (1+ (point-at-eol)))
               (oref it end)))
    (message "Copied hunk: %s" (oref it value))))

(defun km/magit-copy-as-kill ()
  "Try `km/magit-copy-functions' before calling `magit-copy-section-value'.
With a prefix argument of -1, always call `magit-copy-section-value'
Otherwise, the current prefix argument is passed to each hook
function."
  (interactive)
  (or (unless (= (prefix-numeric-value current-prefix-arg) -1)
        (run-hook-with-args-until-success
         'km/magit-copy-functions current-prefix-arg))
      (magit-copy-section-value)))


;;; GitHub links

(defun km/magit-github-url ()
  ;; `bug-reference-url-format' may be defined in an untracked
  ;; `.dir-locals.el`, so do this from the main worktree.
  (-when-let* ((wtree (caar (magit-list-worktrees)))
               (url
                (with-temp-buffer
                  (let ((default-directory (file-name-as-directory wtree)))
                    (hack-dir-local-variables-non-file-buffer)
                    bug-reference-url-format))))
    (and url
         (string-match "\\`https://github.com/[^/]+/[^/]+" url)
         (match-string 0 url))))

(defun km/magit-github-file-link ()
  (let ((ln (lambda (loc)
              (save-restriction
                (1+ (count-lines (point-min) loc))))))
    (-when-let* ((rev (or magit-buffer-revision
                          (magit-rev-parse "HEAD")))
                 (fname (magit-file-relative-name))
                 (lines (if (use-region-p)
                            (prog1 (format "%s-L%s"
                                           (funcall ln (region-beginning))
                                           (1- (funcall ln (region-end))))
                              (deactivate-mark))
                          (funcall ln (point)))))
      (format "%s/blob/%s/%s#L%s"
              (or (km/magit-github-url) "")
              rev fname lines))))

(defun km/magit-github-commit-link ()
  (--when-let (or (and (eq major-mode 'magit-revision-mode)
                       (car magit-refresh-args))
                  (and (derived-mode-p 'magit-mode)
                       (let ((sec (magit-current-section)))
                         (and (eq (oref sec type) 'commit)
                              (oref sec value)))))
    (format "%s/commit/%s"
            (or (km/magit-github-url) "")
            (magit-rev-parse it))))

(defun km/magit-github-diff-link ()
  (when (derived-mode-p 'magit-diff-mode)
    (let ((range (car magit-refresh-args)))
      (when (and range
                 (string-match magit-range-re range))
        ;; This always converts to commits IDs.  It could try to map
        ;; refnames to the appropriate GitHub link (including remotes
        ;; to forks), but I don't have much need for it at the moment.
        (let ((rev1 (magit-rev-parse (match-string 1 range)))
              (rev2 (magit-rev-parse (match-string 3 range))))
          (format "%s/compare/%s...%s"
                  (or (km/magit-github-url) "")
                  rev1 rev2))))))

(defun km/magit-copy-github-link ()
  "Copy a GitHub link from the current file or Magit buffer.

Note: There are probably a number of packages that provide more
complete support for this sort of functionality.  This is just a
simple solution that works for me."
  (interactive)
  (kill-new
   (message "%s"
            (or (km/magit-github-file-link)
                (km/magit-github-commit-link)
                (km/magit-github-diff-link)
                (user-error "Don't know how to make a link from here")))))

(defun km/magit-open-github-pr ()
  (interactive)
  (browse-url
   (format "%s/compare/master...kyleam:%s?expand=1"
           (or (km/magit-github-url)
               (user-error "Couldn't determine GitHub URL"))
           (let ((current (magit-get-current-branch)))
             (if (member (concat "kyleam/" current)
                         (magit-list-refnames "refs/remotes/kyleam"))
                 current
               (magit-read-remote-branch "PR branch" "kyleam"))))))


;;; Git Rebase mode

(defun km/git-rebase--clean-subject (s)
  (replace-regexp-in-string
   (concat "\\`" (regexp-opt '("fixup! " "squash! "))) "" s))

(defun km/git-rebase-fixup-duplicates (beg end &optional squash)
  "Mark sequential lines with same subject as fixup commits.
With an active region, limit to lines that the region touches.
If prefix argument SQUASH is non-nil, mark for squashing instead
of fixing up."
  (interactive (nconc (km/region-or-buffer-line-bounds)
                      (list current-prefix-arg)))
  (unless (markerp end)
    (setq end (copy-marker end)))
  (save-excursion
    (goto-char beg)
    (let ((prefix (if squash "squash" "fixup"))
          prev-subj subj)
      (while (re-search-forward git-rebase-line end t)
        (setq subj (km/git-rebase--clean-subject
                    (match-string-no-properties 3)))
        (when (equal subj prev-subj)
          (let ((inhibit-read-only t))
            (replace-match prefix 'fixedcase nil nil 1)))
        (setq prev-subj subj)))))

(defun km/git-rebase-join-repeats (beg end &optional arg)
  "Move repeated subject lines after line of first occurrence.

If region is active, limit to lines that the region touches.

By default, repeated lines are marked for fixing up.
With a \\[universal-argument], mark them for squashing instead.
With a \\[universal-argument] \\[universal-argument], do not mark them at all."
  (interactive (nconc (km/region-or-buffer-line-bounds)
                      (list current-prefix-arg)))
  (save-excursion
    (goto-char beg)
    (let (roots dups)
      (while (re-search-forward git-rebase-line end t)
        (let ((subj (km/git-rebase--clean-subject
                     (match-string-no-properties 3))))
          (push (list subj (match-string-no-properties 0) (point-marker))
                (if (assoc subj roots) dups roots))))
      (pcase-dolist (`(,subj ,line ,marker) dups)
        (goto-char (1+ (nth 2 (assoc subj roots))))
        (let ((inhibit-read-only t))
          (insert (concat line "\n"))
          (goto-char marker)
          (delete-region (point-at-bol) (1+ (point-at-eol)))))
      (unless (equal arg (list 16))
        (km/git-rebase-fixup-duplicates beg end (equal arg (list 4)))))))

(defun km/git-rebase--move-line (new-pos)
  (let ((ln-beg (point-at-bol))
        (ln-end (1+ (point-at-eol))))
    (goto-char new-pos)
    (let ((inhibit-read-only t))
      (insert (delete-and-extract-region ln-beg ln-end)))
    (forward-line -1)))

(defun km/git-rebase--collect-lines ()
  (let ((current-ln (point-at-bol))
        pt candidates)
    (save-excursion
      (save-restriction
        (narrow-to-region (window-start) (window-end))
        (goto-char (point-min))
        (while (re-search-forward git-rebase-line nil t)
          (setq pt (point-at-bol))
          (unless (= pt current-ln)
            (push pt candidates)))
        ;; Offer first empty line after last commit as candidate so
        ;; the current commit can be moved to the end.
        (unless (> current-ln (car candidates))
          (forward-line 1)
          (push (point-at-bol) candidates))
        (nreverse candidates)))))

(defun km/git-rebase-move-commit ()
  "Move the commit on current line above selected line."
  (interactive)
  (unless (save-excursion (beginning-of-line)
                          (looking-at-p git-rebase-line))
    (user-error "Not on commit line"))
  (avy-with km/git-rebase-move-commit
    (setq avy-action #'km/git-rebase--move-line)
    (avy--process
     (km/git-rebase--collect-lines)
     #'avy--overlay-post)))

(provide 'km-magit)
;;; km-magit.el ends here
