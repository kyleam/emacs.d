;;;; org-mode
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "|" "DONE" "NA")))

                                        ;(setq org-agenda-files (list "~/notes/tasks.org"))

;; next 6 lines merged from prelude-org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

;; allow for file to end in txt
(add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))

;; set up capture
;; (setq org-default-notes-file (expand-file-name "~/notes/tasks.org"))
(setq org-capture-templates
      '(("c" "cal" entry (file+headline "~/notes/calendar.org" "misc")
         "* %?")
        ("t" "Todo" entry (file+headline "~/notes/tasks.org" "To file")
         "* TODO %?")
        ("m" "mail todo" entry (file+headline "~/notes/tasks.org" "mail")
         "* TODO %?\nSource: %u, %c\n%i")
        ("d" "mail date" entry (file+headline "~/notes/calendar.org" "mail")
         "* %?\nSource: %u, %c\n%i")))

;; for mutt capture
(require 'org-protocol)
;; ensure that emacsclient will show just the note to be edited when invoked
;; from Mutt, and that it will shut down emacsclient once finished;
;; fallback to legacy behavior when not invoked via org-protocol.
(add-hook 'org-capture-mode-hook 'delete-other-windows)
(setq my-org-protocol-flag nil)
(defadvice org-capture-finalize (after delete-frame-at-end activate)
  "Delete frame at capture finalization"
  (progn (if my-org-protocol-flag (delete-frame))
         (setq my-org-protocol-flag nil)))
(defadvice org-capture-kill (after delete-frame-at-end activate)
  "Delete frame at capture abort"
  (progn (if my-org-protocol-flag (delete-frame))
         (setq my-org-protocol-flag nil)))
(defadvice org-protocol-capture (before set-org-protocol-flag activate)
  (setq my-org-protocol-flag t))

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
           ("u" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                          (quote regexp) "\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: ")))))))

;; other customization in prelude's org module

;; have font colors in code blocks
(setq org-src-fontify-natively t)

;; from http://doc.norang.ca/org-mode.html
;; this should prevent blank lines from being inserted
;; before headers
(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))
(setq org-cycle-separator-lines 1)


;; from http://doc.norang.ca/org-mode.html
(setq org-refile-targets (quote ((nil :maxlevel . 5)
                                 (org-agenda-files :maxlevel . 5))))

;; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path nil)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

;;;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun km/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'km/verify-refile-target)

;; set up babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '( (perl . t)
    (sh . t)
    (python . t)
    (R . t)
    (emacs-lisp . t)
    (latex . t)
    (ditaa . t)
    ))
;; don't ask for confirmation before running code
(setq org-confirm-babel-evaluate nil)

;; babel minted latex export
;; modified from
;; http://orgmode.org/worg/org-tutorials/org-latex-export.html
(setq org-export-latex-listings 'minted)
(setq org-export-latex-custom-lang-environments
      '(
        (R "rcode")
        (sh "shcode")
        (python "pythoncode")
        ))
;; (setq org-export-latex-custom-lang-environments
;;       '(
;;         (emacs-lisp "common-lispcode")
;;         ))
;; (setq org-export-latex-minted-options
;;       '(("frame" "lines")
;;         ("fontsize" "\\scriptsize")
;;         ("linenos" "")))
(setq org-latex-to-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
