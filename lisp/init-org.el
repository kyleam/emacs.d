(add-to-list 'load-path "~/src/emacs/org-mode/lisp")

(setq org-modules '(org-bibtex org-gnus org-info)
      org-log-done t
      org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "NA(@)"))
      org-use-speed-commands t
      org-fast-tag-selection-single-key 'expert
      org-catch-invisible-edits  'error
      org-goto-interface 'outline-path-completionp
      org-src-fontify-natively t
      org-special-ctrl-k t
      org-outline-path-complete-in-steps nil
      org-completion-use-ido t
      org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

(setq org-capture-templates
      '(("t" "task" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%i")
        ("d" "date" entry (file+headline "~/notes/calendar.org" "Inbox")
         "* %?\n%i")
        ;; Link counterparts
        ("T" "task link" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%i\nLink: %a")
        ("D" "date link" entry (file+headline "~/notes/calendar.org" "Inbox")
         "* %?\n%i\nLink: %a")
        ;; Clipboard
        ("x" "task clipboard" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%x")))
(key-chord-define-global ",t" 'org-capture)

(defun km/open-main-orgfile ()
  (interactive)
  (find-file org-default-notes-file))

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o b") 'org-iswitchb)
(global-set-key (kbd "C-c o s") 'org-save-all-org-buffers)
(global-set-key (kbd "C-c o m") 'km/open-main-orgfile)
(key-chord-define-global ",a" 'org-agenda)

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

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-c C-x B")
       'km/org-tree-to-indirect-buffer-current-window)
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
     (add-to-list 'org-latex-packages-alist '("" "amsmath" t))))

(add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))

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
        starting-pos
        chars-after-heading)
    (setq starting-pos (point))
    (save-excursion
      (org-back-to-heading t)
      (setq chars-after-heading (- starting-pos (point)))
      (outline-up-heading arg)
      (call-interactively 'org-sort))
    (goto-char (+ (org-find-exact-headline-in-buffer heading nil t)
                  chars-after-heading))))

;;; Org in other modes
(defun km/load-orgstruct ()
  (turn-on-orgstruct++)
  (turn-on-orgtbl))

(add-hook 'message-mode-hook 'km/load-orgstruct)
(add-hook 'git-commit-mode-hook 'km/load-orgstruct)

(eval-after-load 'org
     '(diminish 'orgstruct-mode "Os"))
(eval-after-load 'org-table
     '(diminish 'orgtbl-mode "Ot"))

(add-hook 'next-error-hook '(lambda ()
                              (when (eq major-mode 'org-mode)
                                (org-show-context))))

(defadvice magit-visit
  (after magit-visit-show-org-context activate)
  "Show context if visiting Org buffer.
This is an improvement, but still not great. It requires the
point be on or under a heading in the Magit diff. If above, the
context will be shown for above heading."
  (when (eq major-mode 'org-mode)
    (org-show-context)))

;;; Agenda

(defadvice org-agenda-list (around org-agenda-fullscreen activate)
  "Start agenda in fullscreen.

After agenda loads, delete other windows.
`org-agenda-restore-windows-after-quit' should non-nil to restore
the previous window configuration. If `org-agenda-sticky' is
non-nil, configurations with more than one window do not seem to
be restored properly."
  ad-do-it
  (delete-other-windows))

(setq org-agenda-restore-windows-after-quit t
      org-agenda-sticky nil)

(setq org-agenda-files '("~/notes/calendar.org" "~/notes/tasks.org")
      org-default-notes-file "~/notes/tasks.org"
      org-agenda-text-search-extra-files '("~/notes/backburner.org"
                                           "~/notes/misc.org")
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-reverse-note-order t)

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

(setq org-refile-targets `((nil :maxlevel . 3)
                           (,(append org-agenda-files
                                     org-agenda-text-search-extra-files)
                            :maxlevel . 2)))

(setq org-refile-target-verify-function 'km/verify-refile-target)

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

(defun km/org-set-refiling-buffer (maxlevel)
  "Choose buffer to set as sole target in `org-refile-targets'.
If `org-refile-targets' is already a local variable, restore the
global value. A numeric prefix can be given to set
MAXLEVEL (defaults to 2)."
  (interactive "P")
  (if (local-variable-p 'org-refile-targets)
      (kill-local-variable 'org-refile-targets)
    (let* ((buffer-file (substring-no-properties
                         (buffer-file-name (km/get-org-file-buffer))))
          (maxlevel (prefix-numeric-value (or maxlevel 2))))
      (set (make-local-variable 'org-refile-targets)
           `((,buffer-file :maxlevel . ,maxlevel))))))

(eval-after-load 'org
  '(add-to-list 'org-mode-hook
                '(lambda ()
                   (local-set-key (kbd "C-c m w")
                                  'km/org-refile-to-other-org-buffer)
                   (local-set-key (kbd "C-c m W")
                                  'km/org-refile-to-other-file)
                   (local-set-key (kbd "C-c m s")
                                  'km/org-sort-parent)
                   (local-set-key (kbd "C-c m o")
                                  'ace-link-org))))

(provide 'init-org)
