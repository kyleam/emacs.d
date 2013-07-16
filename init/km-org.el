;;;; org-mode
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "|" "DONE" "NA")))

;; set up capture
(setq org-capture-templates
      '(("t" "task" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%i")
        ("d" "date" entry (file+headline "~/notes/calendar.org" "Inbox")
         "* %?\n%i")
        ("m" "misc" entry (file+headline "~/notes/misc.org" "Inbox")
         "* %?\n%i")
        ("b" "backburner" entry (file+headline "~/notes/backburner.org" "Inbox")
         "* TODO %?\n%i")
        ;; link counterparts
        ("T" "task link" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%i\nLink: %a")
        ("D" "date link" entry (file+headline "~/notes/calendar.org" "Inbox")
         "* %?\n%i\nLink: %a")
        ("M" "misc link" entry (file+headline "~/notes/misc.org" "Inbox")
         "* %?\n%i\nLink: %a")
        ("B" "backburner link" entry (file+headline "~/notes/backburner.org" "Inbox")
         "* %?\n%i\nLink: %a")
        ;; clipboard
        ("x" "task clipboard" entry (file+headline "~/notes/tasks.org" "Inbox")
         "* TODO %?\n%x")
        ("X" "misc clipboard" entry (file+headline "~/notes/misc.org" "Inbox")
         "* %?\n%x")))

(custom-set-variables
 '(org-agenda-files (quote ("~/notes/calendar.org" "~/notes/tasks.org")))
 '(org-default-notes-file "~/notes/tasks.org")
 '(org-agenda-ndays 7)
;; '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("d" todo "DONE" nil)
           ("s" todo "STARTED" nil)
           ("A" agenda ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
             (org-agenda-ndays 1)
             (org-agenda-overriding-header "Today's Priority #A tasks: ")))
           ("u" "Unschedule TODO entries" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                          (quote regexp) "\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: ")))
           ("p" "Past timestamps" tags "TIMESTAMP<=\"<now>\"")))))

(defvar km/org-additional-notes-files '("~/notes/backburner.org")
  "non-agenda files that should be available for refiling")

(defvar km/org-refiling-targets
  (append km/org-additional-notes-files org-agenda-files))

;; other customization in prelude's org module

;; have font colors in code blocks
(setq org-src-fontify-natively t)

;; from http://doc.norang.ca/org-mode.html
;; this should prevent blank lines from being inserted
;; before headers
(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))
(setq org-cycle-separator-lines 1)



;;;; Refile settings

;; from http://doc.norang.ca/org-mode.html
(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (km/org-refiling-targets :maxlevel . 2))))

;; use IDO for refiling
(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido t)

;; Exclude DONE state tasks from refile targets
(defun km/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'km/verify-refile-target)

(defun km/open-main-orgfile ()
  (interactive)
  (find-file org-default-notes-file))

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o b") 'org-iswitchb)
(global-set-key (kbd "C-c o m") 'km/open-main-orgfile)

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
        ;; lower case versions of defaults
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


;; don't let `org-cycle-agenda-files' binding override custom
;; `backward-kill-word' binding (`org-cycle-agenda-files' is still bound
;; to C-,)
(define-key org-mode-map (kbd "C-'") nil)

(define-key org-mode-map (kbd "C-c o j") 'org-metadown)
(define-key org-mode-map (kbd "C-c o k") 'org-metaup)
(define-key org-mode-map (kbd "C-c o h") 'org-metaleft)
(define-key org-mode-map (kbd "C-c o H") 'org-shiftmetaleft)
(define-key org-mode-map (kbd "C-c o l") 'org-metaright)
(define-key org-mode-map (kbd "C-c o L") 'org-shiftmetaright)

(setq org-use-speed-commands t)

(setq org-catch-invisible-edits  'error)
