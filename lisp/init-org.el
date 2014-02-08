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
      org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

(setq org-capture-templates
      '(("t" "task" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%i")
        ("d" "date" entry (file+headline "~/notes/calendar.org" "Inbox")
         "* %?\n%i")
        ("m" "misc" entry (file+headline "~/notes/misc.org" "Inbox")
         "* %?\n%i")
        ("b" "backburner" entry (file+headline "~/notes/backburner.org" "Inbox")
         "* TODO %?\n%i")
        ;; Link counterparts
        ("T" "task link" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%i\nLink: %a")
        ("D" "date link" entry (file+headline "~/notes/calendar.org" "Inbox")
         "* %?\n%i\nLink: %a")
        ("M" "misc link" entry (file+headline "~/notes/misc.org" "Inbox")
         "* %?\n%i\nLink: %a")
        ("B" "backburner link" entry (file+headline "~/notes/backburner.org" "Inbox")
         "* TODO %?\n%i\nLink: %a")
        ;; Clipboard
        ("x" "task clipboard" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%x")
        ("X" "misc clipboard" entry (file+headline "~/notes/misc.org" "Inbox")
         "* %?\n%x")))
(key-chord-define-global ",t" 'org-capture)

(defun km/open-main-orgfile ()
  (interactive)
  (find-file org-default-notes-file))

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o b") 'org-iswitchb)
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
     ;; Don't let `org-cycle-agenda-files' binding override custom
     ;; `backward-kill-word' binding (`org-cycle-agenda-files' is still bound
     ;; to C-,).
     (define-key org-mode-map (kbd "C-'") nil)
     ;; Bog keybindings
     (define-prefix-command 'bog-map)
     (define-key org-mode-map (kbd "C-c b") 'bog-map)
     (define-key bog-map "p" 'bog-find-citekey-pdf)
     (define-key bog-map "b" 'bog-find-citekey-bib)
     (define-key bog-map "w" 'bog-search-citekey-on-web)
     ;; Avoid conflict when amsmath is loaded.
     (setcar (rassoc '("wasysym" t) org-latex-default-packages-alist)
             "nointegrals")
     (add-to-list 'org-latex-packages-alist '("" "amsmath" t))))

(add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))

;;; Org in other modes
(defun km/load-orgstruct ()
  (turn-on-orgstruct++)
  (turn-on-orgtbl))

(add-hook 'message-mode-hook 'km/load-orgstruct)
(add-hook 'git-commit-mode-hook 'km/load-orgstruct)


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

;;; Agenda

(setq org-agenda-files '("~/notes/calendar.org" "~/notes/tasks.org")
      org-default-notes-file "~/notes/tasks.org"
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-reverse-note-order t)

(setq org-agenda-custom-commands
      '(("d" todo "DONE" nil)
        ("s" todo "STARTED" nil)
        ("u" "Unschedule TODO entries" alltodo ""
         ((org-agenda-skip-function
           (lambda nil
             (org-agenda-skip-entry-if 'scheduled 'deadline
                                       'regexp "\n]+>")))
          (org-agenda-overriding-header "Unscheduled TODO entries: ")))
        ("p" "Past timestamps" tags "TIMESTAMP<=\"<now>\"")))

;;; Refiling

(defvar km/org-refiling-targets
  (append org-agenda-files '("~/notes/backburner.org" "~/notes/misc.org"))
  "List of refiling targets for agenda, including non-agenda
files.")

(defun km/verify-refile-target ()
  "Exclude DONE state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-targets '((nil :maxlevel . 3)
                           (km/org-refiling-targets :maxlevel . 2)))
(setq org-refile-target-verify-function 'km/verify-refile-target
      ;; Use ido for refiling.
      org-outline-path-complete-in-steps nil
      org-completion-use-ido t)

(provide 'init-org)
