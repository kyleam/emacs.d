(add-to-list 'load-path "~/src/emacs/org-mode/lisp/")
(add-to-list 'load-path "~/src/emacs/org-mode/contrib/lisp/" t)
(add-to-list 'Info-directory-list "~/src/emacs/org-mode/doc/")

(setq org-modules '(org-bibtex org-gnus org-info)
      org-log-done t
      org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@)"
                                    "|" "DONE(d)" "NA(n@)"))
      org-log-into-drawer t
      org-clock-into-drawer t
      org-use-speed-commands t
      org-fast-tag-selection-single-key 'expert
      org-catch-invisible-edits  'error
      org-goto-interface 'outline-path-completionp
      org-src-fontify-natively t
      org-special-ctrl-k t
      org-outline-path-complete-in-steps nil
      org-completion-use-ido t
      org-reverse-note-order t
      org-link-search-must-match-exact-headline nil
      org-insert-heading-respect-content t
      org-M-RET-may-split-line nil
      org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

(setq org-capture-templates
      '(("t" "task" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%i")
        ("d" "date" entry (file+headline "~/notes/calendar.org" "Inbox")
         "* %?\n%i")
        ("m" "misc" entry (file+headline "~/notes/misc.org" "Inbox")
         "* %?\n%i")
        ;; Link counterparts
        ("T" "task link" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%i\nLink: %a")
        ("D" "date link" entry (file+headline "~/notes/calendar.org" "Inbox")
         "* %?\n%i\nLink: %a")
        ("M" "misc link" entry (file+headline "~/notes/misc.org" "Inbox")
         "* %?\n%i\nLink: %a")
        ;; Clipboard
        ("x" "task clipboard" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%x")
        ("X" "misc clipboard" entry (file+headline "~/notes/misc.org" "Inbox")
         "* %?\n%x")))
(key-chord-define-global ",t" 'org-capture)

(defadvice org-open-file (after km/org-open-add-to-recentf activate)
  (recentf-add-file path))

(defun km/org-open-file-at-point ()
  "Open file at point with `org-open-file'."
  (interactive)
  (let ((file (thing-at-point 'filename)))
    (if (and file (file-exists-p file))
        (org-open-file file)
      (user-error "No file at point"))))

(defun km/org-open-file ()
  "Interactive version of `org-open-file'."
  (interactive)
  (org-open-file (read-file-name "Open file: " nil nil t)))

(defun km/org-open-annex-file ()
  "Open a git annex file with `org-open-file'."
  (interactive)
  (--if-let (magit-annex-present-files)
      (org-open-file (magit-completing-read "Open annex file" it nil t))
    (message "No annex files found")))

(defun km/org-open-recent-file ()
  "Open a file from `recentf-list' with `org-open-file'."
  (interactive)
  (org-open-file (km/read-recent-file)))

(autoload 'magit-annex-present-files "magit-annex")

(after 'init-buffile
  (define-key km/file-map "a" 'km/org-open-annex-file)
  (define-key km/file-map "o" 'km/org-open-file)
  (define-key km/file-map "p" 'km/org-open-file-at-point)
  (define-key km/file-map "r" 'km/org-open-recent-file))

(define-prefix-command 'km/global-org-map)
(global-set-key (kbd "C-c o") 'km/global-org-map)

(defvar km/org-store-link-hook nil
  "Hook run before by `km/org-store-link-hook'.
These are run within a `save-window-excursion' block.")

(defun km/org-store-link ()
  "Run `km/org-store-link-hook' before `org-store-link'.
The hook functions and `org-store-link' are called within a
`save-window-excursion' block."
  (interactive)
  (save-window-excursion
    (run-hooks 'km/org-store-link-hook)
    (call-interactively 'org-store-link)))

(define-key km/global-org-map "l" 'km/org-store-link)
(define-key km/global-org-map "o" 'org-open-at-point-global)
(define-key km/global-org-map "a" 'org-agenda)
(define-key km/global-org-map "b" 'org-iswitchb)
(define-key km/global-org-map "s" 'org-save-all-org-buffers)
(define-key km/global-org-map "p" 'poporg-dwim)
(key-chord-define-global ",a" 'org-agenda)

(after 'poporg
  (define-key poporg-mode-map (kbd "C-c C-c") 'poporg-edit-exit))

(setq org-structure-template-alist
      '(("p" "#+property: " "")
        ("o" "#+options: " "")
        ("d" "#+date: " "")
        ("t" "#+title: " "")
        ("S" "#+setupfile: ?" "")
        ("n" "#+begin_note\n  ?\n#+end_note" "<note>\n?\n</note>")
        ("w" "#+begin_note\n  ?\n#+end_note" "<note>\n?\n</note>")
        ("C" "#+caption: " "")
        ("b" "#+label: " "")
        ("r" "#+attr_latex: " "")
        ("R" "#+attr_html: " "")
        ;; Lower case versions of defaults
        ("s" "#+begin_src ?\n  \n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n  ?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n  ?\n#+end_quote" "<quote>\n?\n</quote>")
        ("v" "#+begin_versen  ?\n#+end_verse" "<verse>\n?\n</verse>")
        ("V" "#+begin_verbatim\n  ?\n#+end_verbatim" "<verbatim>\n?\n</verbatim>")
        ("c" "#+begin_center\n  ?\n#+end_center" "<center>\n?\n</center>")
        ("l" "#+begin_latex\n  ?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
        ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n  ?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
        ("H" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n  ?\n#+end_ascii" "")
        ("A" "#+ascii: " "")
        ("i" "#+index: ?" "#+index: ?")
        ("I" "#+include: %file ?" "<include file=%file markup=\"?\">")))

(define-prefix-command 'km/org-prefix-map)
(define-key km/org-prefix-map "w" 'km/org-refile-to-other-org-buffer)
(define-key km/org-prefix-map "s" 'km/org-sort-parent)
(define-key km/org-prefix-map "l" 'km/org-remove-title-leader)

(after 'org
  (define-key org-mode-map (kbd "C-c C-x B")
    'km/org-tree-to-indirect-buffer-current-window)
  (define-key org-mode-map (kbd "C-c m") 'km/org-prefix-map)
  ;; Override global `imenu' binding.
  (define-key org-mode-map (kbd "C-c j") 'org-goto)
  ;; Don't let `org-cycle-agenda-files' binding override custom
  ;; `backward-kill-word' binding (`org-cycle-agenda-files' is still bound
  ;; to C-,).
  (define-key org-mode-map (kbd "C-'") nil)
  ;; Rebind `org-insert-drawer' to so that `org-metadown' has the
  ;; expected "C-c C-x" keybinding.
  (define-key org-mode-map (kbd "C-c C-x d") 'org-metadown)
  (define-key org-mode-map (kbd "C-c C-x w") 'org-insert-drawer)
  ;; Avoid conflict when amsmath is loaded.
  (setcar (rassoc '("wasysym" t) org-latex-default-packages-alist)
          "nointegrals")
  (add-to-list 'org-latex-packages-alist '("" "amsmath" t)))

(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

(add-to-list 'safe-local-variable-values '(org-goto-max-level . 2))

(defadvice org-tree-to-indirect-buffer (before
                                        org-keep-previous-indirect
                                        (&optional arg)
                                        activate)
  "Retain previous indirect buffer from `org-tree-to-indirect-buffer'.
By default, `org-tree-to-indirect-buffer' deletes the previous
indirect buffer when making a new one to avoid accumulating
buffers, which can be overriden by a C-u prefix. This advice
reverses this behavior so that the prefix must be given in order
to delete the previous indirect buffer. If the argument is a
number, which has a different meaning, it is left untouched."
  (unless (numberp arg)
    (setq arg (not arg))))

(defun km/org-tree-to-indirect-buffer-current-window (&optional arg)
  "Create indirect buffer and narrow to subtree in this window.
Before running `org-tree-to-indirect-buffer',
`org-indirect-buffer-display' is set to `current-window'."
  (interactive "P")
  (let ((org-indirect-buffer-display 'current-window))
    (org-tree-to-indirect-buffer arg)))

(defun km/org-sort-parent (arg)
  "Sort on parent heading ARG levels up.
After sorting, the point is returned to its previous location
under the current heading."
  (interactive "p")
  (let ((heading (org-no-properties (org-get-heading t t)))
        (starting-pos (point))
        chars-after-heading)
    (org-back-to-heading t)
    (setq chars-after-heading (- starting-pos (point)))
    (outline-up-heading arg)
    (call-interactively #'org-sort)
    ;; Sorting doesn't play well with `save-restriction' or markers,
    ;; so just put the point where it was relative to the original
    ;; heading.  This may not actually be the same tree if there are
    ;; redundant headings.
    (goto-char (+ (org-find-exact-headline-in-buffer heading nil t)
                  chars-after-heading))))

(defun km/org-sort-heading-ignoring-articles ()
  "Sort alphabetically, but ignore any leading articles."
  (when (looking-at org-complex-heading-regexp)
    (let ((ignored-words '("a" "an" "the"))
          heading heading-words)
      (setq heading
            (funcall 'downcase
                     (org-sort-remove-invisible (match-string 4))))
      (setq heading-words (split-string heading))
      (when (member (car heading-words) ignored-words)
        (setq heading-words (cdr heading-words)))
      (mapconcat 'identity heading-words " "))))

(defun km/org-remove-title-leader ()
  "Remove leader from Org heading title.

Convert

  * TODO leader: Rest of title       :tag:

to

  * TODO Rest of title               :tag:"
  (interactive)
  (save-excursion
    (let ((regex (format "^%s\\(?:%s \\)?\\(?:%s \\)?\\(.*: \\)\\w+"
                         org-outline-regexp org-todo-regexp
                         org-priority-regexp)))
      (org-back-to-heading)
      (when (re-search-forward regex (point-at-eol) t)
        (replace-match "" nil nil nil 4)
        (org-set-tags nil t)))))

(defun km/org-add-blank-before-heading ()
  "Add a blank line before Org headings in buffer."
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (while (re-search-forward "[^\n]\n\\*" nil t)
     (when (org-at-heading-p)
       (beginning-of-line)
       (open-line 1)))))

(defun km/org-normalize-spaces ()
  "Reduce to single spaces and add space before headings."
  (interactive)
  (km/reduce-to-single-spaces)
  (km/org-add-blank-before-heading))

(define-key km/org-prefix-map "n" 'km/org-normalize-spaces)

;;; Org in other modes
(defun km/load-orgstruct ()
  (turn-on-orgstruct++)
  (turn-on-orgtbl))

(add-hook 'message-mode-hook 'km/load-orgstruct)

(after 'git-commit
  (add-hook 'git-commit-setup-hook 'km/load-orgstruct))

(add-hook 'next-error-hook (lambda ()
                             (when (eq major-mode 'org-mode)
                               (org-show-context))))

;;; Agenda

(after 'org-agenda
  (defadvice org-agenda-list (around org-agenda-fullscreen activate)
    "Start agenda in fullscreen.

After agenda loads, delete other windows.
`org-agenda-restore-windows-after-quit' should non-nil to restore
the previous window configuration. If `org-agenda-sticky' is
non-nil, configurations with more than one window do not seem to
be restored properly."
    ad-do-it
    (delete-other-windows)))

(defun km/org-agenda-cd-and-read-dir-locals ()
  (setq default-directory "~/notes/")
  (hack-local-variables))

(add-hook 'org-agenda-mode-hook 'km/org-agenda-cd-and-read-dir-locals)

(setq org-agenda-restore-windows-after-quit t
      org-agenda-sticky nil)

(setq org-default-notes-file "~/notes/agenda/tasks.org"
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-dim-blocked-tasks nil
      org-agenda-use-time-grid nil
      org-agenda-start-on-weekday nil)

(setq org-agenda-sorting-strategy
      '((agenda time-up deadline-up scheduled-up priority-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

(defvar km/org-agenda-file-directory "~/notes/agenda/")
(setq org-agenda-files (list km/org-agenda-file-directory))
(setq org-agenda-text-search-extra-files
      (file-expand-wildcards "~/notes/extra/*.org"))

(defun km/org-agenda-add-or-remove-file (file)
  "Add or remove link to FILE in `km/org-agenda-file-directory'.
If a link for FILE does not exist, create it. Otherwise, remove
it. Like `org-agenda-file-to-front', this results in FILE being
displayed in the agenda."
  (interactive (list (case major-mode
                       (org-mode (buffer-file-name))
                       (dired-mode (dired-get-filename))
                       (org-agenda-mode (ignore-errors (save-window-excursion
                                                         (org-agenda-goto)
                                                         (buffer-file-name))))
                       (t (read-file-name "Link file: ")))))
  (let ((agenda-file (expand-file-name (file-name-nondirectory file)
                                       km/org-agenda-file-directory)))
    (if (file-equal-p (file-truename agenda-file) file)
        (progn
          (when (called-interactively-p) (message "Deleting %s" agenda-file))
          (delete-file agenda-file))
      (when (called-interactively-p) (message "Adding %s" agenda-file))
      (make-symbolic-link file agenda-file))))

(define-key km/global-org-map "n" 'km/org-agenda-add-or-remove-file)

(defun km/org-open-default-notes-file-inbox ()
  "Open \"Inbox\" heading of `org-default-notes-file'."
  (interactive)
  (find-file org-default-notes-file)
  (goto-char (org-find-exact-headline-in-buffer "Inbox" nil t))
  (recenter-top-bottom 0)
  (show-children))

(define-key km/global-org-map "m" 'km/org-open-default-notes-file-inbox)

(setq org-agenda-custom-commands
      '(("d" todo "DONE" nil)
        ("u" "Unschedule TODO entries" alltodo ""
         ((org-agenda-skip-function
           (lambda nil
             (org-agenda-skip-entry-if 'scheduled 'deadline
                                       'regexp "\n]+>")))
          (org-agenda-overriding-header "Unscheduled TODO entries: ")))
        ("p" "Past timestamps" tags "TIMESTAMP<=\"<now>\"")))

;;; Refiling

(defun km/verify-refile-target ()
  "Exclude DONE state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-targets '((nil :maxlevel . 2))
      org-refile-cache nil)

(add-to-list 'safe-local-variable-values
             '(org-refile-targets
               (nil :maxlevel . 3)
               (org-agenda-files :maxlevel . 2)
               (org-agenda-text-search-extra-files :maxlevel . 2)))

(setq org-refile-target-verify-function 'km/verify-refile-target)

(defadvice org-refile (around km/org-refile-dwim activate)
  "Rebind `org-refile-targets' if next window is an Org buffer.
A target is determined by `km/org-refile-dwim-target-file'."
  (let* ((dwim-target (km/org-refile-dwim-target-file))
         (org-refile-targets (if dwim-target
                                 `((nil
                                    :maxlevel . ,km/org-refile-dwim-maxlevel)
                                   (dwim-target
                                    :maxlevel . ,km/org-refile-dwim-maxlevel))
                               org-refile-targets)))
    ad-do-it))

(defun km/org-refile-dwim-target-file ()
  "Return next window that is an Org buffer."
  (let ((from-buffer (current-buffer)))
    (--when-let (get-window-with-predicate
                 (lambda (w)
                   (with-current-buffer (window-buffer w)
                     (and (eq major-mode 'org-mode)
                          (not (eq from-buffer (current-buffer)))))))
      (buffer-file-name (window-buffer it)))))

(defvar km/org-refile-dwim-maxlevel 2)

(defun km/org-refile-to-other-file (file &optional maxlevel)
  "Refile with `org-refile-targets' set to FILE.
A numeric prefix can be given to set MAXLEVEL (defaults to 2)."
  (interactive "fFile: \nP")
  (let* ((maxlevel (prefix-numeric-value (or maxlevel 2)))
         (file (substring-no-properties file))
         (org-refile-targets `((,file :maxlevel . ,maxlevel))))
    (org-refile)))

(defun km/org-refile-to-other-org-buffer (buffer &optional maxlevel)
  "Refile with `org-refile-targets' set to BUFFER file name.
A numeric prefix can be given to set MAXLEVEL (defaults to 2)."
  (interactive (list (km/get-org-file-buffer) current-prefix-arg))
  (let ((buffer-file (buffer-file-name buffer)))
    (km/org-refile-to-other-file buffer-file maxlevel)))

(defun km/get-org-file-buffer ()
  (get-buffer
   (org-icompleting-read "Buffer: " (mapcar 'buffer-name
                                            (org-buffer-list 'files)))))

(defun km/org-set-refiling-buffer (&optional maxlevel)
  "Choose buffer to set as sole target in `org-refile-targets'.
If `org-refile-targets' is already a local variable, restore the
global value. A numeric prefix can be given to set
MAXLEVEL (defaults to 2)."
  (interactive "P")
  (if (local-variable-p 'org-refile-targets)
      (kill-local-variable 'org-refile-targets)
    (let ((buffer-file (substring-no-properties
                        (buffer-file-name (km/get-org-file-buffer))))
          (maxlevel (prefix-numeric-value (or maxlevel 2))))
      (set (make-local-variable 'org-refile-targets)
           `((,buffer-file :maxlevel . ,maxlevel))))))

(defun km/org-switch-to-buffer-other-window (&optional arg)
  (interactive "P")
  (noflet ((org-pop-to-buffer-same-window (&optional buffer-or-name norecord label)
                                          (funcall 'pop-to-buffer buffer-or-name nil norecord)))
    (org-switchb arg)))

(define-key ctl-x-4-map "o" 'km/org-switch-to-buffer-other-window)

;;; Export

(after 'ox-latex
  (add-to-list 'org-latex-classes
               '("short"
                 "\\documentclass{short}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'init-org)
