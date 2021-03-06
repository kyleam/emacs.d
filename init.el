;;; init.el --- Kyle Meyer's Emacs configuration

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

(require 'cl-lib)
(require 'dash)
(require 's)

(require 'bind-key)
(require 'use-package)
(require 'use-package-chords)
(setq use-package-always-defer t)
(key-chord-mode 1)

(defvar km/init-lisp-dir (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path km/init-lisp-dir)
(add-to-list 'custom-theme-load-path km/init-lisp-dir)

(require 'km-util)
(require 'km-emacs-autoloads nil t)

(setq user-full-name "Kyle Meyer")
(setq user-mail-address "kyle@kyleam.com")


;;; Appearance

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq frame-title-format "%b")

(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(column-number-mode)
(line-number-mode)
(size-indication-mode)

(require 'km-theme)


;;; Custom prefix maps

(pcase-dolist (`(,key ,map) '(("C-c c" km/compile-map)
                              ("C-c e" km/editing-map)
                              ("C-c f" km/file-map)
                              ("C-c g" km/git-map)
                              ("C-c k" km/kill-map)
                              ("C-x m" km/mail-map)
                              ("C-c o" km/global-org-map)
                              ("C-c x" km/eval-map)
                              ("C-c z" km/external-map)))
  (define-prefix-command map)
  (global-set-key (kbd key) map))

(define-prefix-command 'km/diff-prefix-map)
(define-prefix-command 'km/dired-copy-filename-map)
(define-prefix-command 'km/dired-narrow-prefix-map)
(define-prefix-command 'km/dired-prefix-map)
(define-prefix-command 'km/dired-subtree-prefix-map)
(define-prefix-command 'km/gnus-summary-prefix-map)
(define-prefix-command 'km/magit-map)
(define-prefix-command 'km/magit-wip-map)
(define-prefix-command 'km/org-prefix-map)
(define-prefix-command 'km/projectile-ctl-x-4-map)
(define-prefix-command 'km/python-prefix-map)
(define-prefix-command 'km/r-prefix-map)
(define-prefix-command 'km/gnus-article-prefix-map)


;;; Org

(use-package org
  :load-path "~/src/emacs/org-mode/lisp/"
  :mode ("\\.org.txt\\'" . org-mode)
  :init
  (require 'org-loaddefs)
  (add-to-list 'load-path "~/src/emacs/org-mode/contrib/lisp/" t)
  (after 'info
    (info-initialize)
    (add-to-list 'Info-directory-list "~/src/emacs/org-mode/doc/"))
  (bind-keys :map km/global-org-map
             ("b" . org-iswitchb)
             ("ci" . (lambda ()
                       (interactive)
                       (org-clock-in '(4))))
             ("cl" . org-clock-in-last)
             ("co" . org-clock-out)
             ("cr" . org-resolve-clocks)
             ("l" . org-store-link)
             ("o" . org-open-at-point)
             ("s" . org-save-all-org-buffers)
             ("w" . org-refile-goto-last-stored))
  (setq org-use-extra-keys t)

  (setq org-export-backends '(ascii html latex))
  :config
  (setq org-log-done t
        org-log-into-drawer t
        org-clock-into-drawer t
        org-clock-out-switch-to-state "WIP"
        org-todo-keywords '((sequence "TODO(t)" "WIP(i)" "WAITING(w@)"
                                      "|" "DONE(d)" "NA(n@)")))

  (put 'org-log-done 'safe-local-variable #'booleanp)
  (put 'org-archive-location 'safe-local-variable #'stringp)

  (setq org-catch-invisible-edits 'error
        org-special-ctrl-k t
        org-insert-heading-respect-content nil
        org-M-RET-may-split-line nil
        org-adapt-indentation nil
        org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
  (setq org-use-speed-commands t
        org-fast-tag-selection-single-key 'expert)
  (setq org-outline-path-complete-in-steps nil
        org-goto-interface 'outline-path-completionp
        org-goto-max-level 3)

  (setq org-default-notes-file "~/notes/agenda/tasks.org")
  (setq org-agenda-text-search-extra-files
        (file-expand-wildcards "~/notes/extra/*.org"))

  (setq org-reverse-note-order t)
  (setq org-refile-targets '((nil :maxlevel . 2))
        org-refile-cache nil)

  (setq org-link-search-must-match-exact-headline nil)

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t)

  (advice-add
   'org-open-file :after
   (lambda (path &rest _) (recentf-add-file path))
   '((name . "recentf-add")))

  (put 'org-goto-max-level 'safe-local-variable #'integerp)

  (add-to-list 'org-latex-packages-alist '("" "amsmath" t))

  (add-hook 'next-error-hook (lambda ()
                               (when (eq major-mode 'org-mode)
                                 (org-show-context))))

  (advice-add
   'org-eval-in-calendar :around
   (lambda (f form &rest _) (funcall f form 'keepdate))
   '((name . "always-keepdate")))

  (advice-add
   'org-refile :around
   (lambda (f &rest args)
     (apply f args)
     (when (bound-and-true-p org-capture-is-refiling)
       (org-save-all-org-buffers)))
   '((name . "org-save-after-capture-refile")))

  (bind-keys :map org-mode-map
             ("C-c l" . org-goto)
             ("C-c m" . km/org-prefix-map)
             ;; Don't let `org-cycle-agenda-files' binding override
             ;; custom `backward-kill-word' binding
             ;; (`org-cycle-agenda-files' is still bound to C-,).
             ("C-'" . nil)
             ("C-c C-x d" . org-metadown)
             ("C-c C-x n" . org-next-item)
             ("C-c C-x s" . org-set-property)
             ("C-c C-x p" . org-previous-item)
             ("C-c C-x w" . org-insert-drawer))

  (define-key km/org-prefix-map "r" #'org-element-cache-reset)

  (require 'org-notmuch))

(use-package ox-latex
  :config
  (add-to-list 'org-latex-classes
               '("short"
                 "\\documentclass{short}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package org-table
  :diminish (orgtbl-mode . "Ot"))

(use-package org-capture
  :init (define-key km/global-org-map "v" #'org-capture)
  :config
  (setq org-capture-templates
        '(("t" "task" entry (file+headline "~/notes/tasks.org" "Inbox")
           "* TODO %?%i" :prepend t)
          ("d" "date" entry (file+headline "~/notes/calendar.org" "Inbox")
           "* %?%i" :prepend t)
          ("b" "bookmark" entry (file+headline "~/notes/bookmarks.org" "Inbox")
           "* %?%i" :prepend t)
          ("v" "Visit" checkitem (file+headline "~/notes/tasks.org" "Visit")
           "- [ ] %?%i\n" :prepend t)
          ("r" "Revisit" checkitem (file+headline "~/notes/tasks.org" "Revisit")
           "- [ ] %?%i\n" :prepend t)
          ;; Link counterparts
          ("T" "task link" entry (file+headline "~/notes/tasks.org" "Inbox")
           "* TODO %?%i\n\n%a" :prepend t)
          ("D" "date link" entry (file+headline "~/notes/calendar.org" "Inbox")
           "* %?%i\n\n%a" :prepend t)
          ("B" "bookmark link" entry
           (file+headline "~/notes/bookmarks.org" "Inbox")
           "* %?%i\n\n%a" :prepend t)
          ;; Clipboard
          ("x" "task clipboard" entry (file+headline "~/notes/tasks.org" "Inbox")
           "* TODO %?%i\n\n%x" :prepend t)
          ("X" "bookmark clipboard" entry
           (file+headline "~/notes/bookmarks.org" "Inbox")
           "* %?%i\n\n%x" :prepend t)))

  (add-hook 'org-capture-before-finalize-hook #'save-buffer)

  (require 'org-agenda)
  (require 'org-contacts))

(use-package org-agenda
  :init (define-key km/global-org-map "a" #'org-agenda)
  :config
  (setq org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'only-window)
  (setq org-agenda-dim-blocked-tasks nil
        org-agenda-inhibit-startup t
        org-agenda-use-tag-inheritance nil
        org-agenda-show-all-dates t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
        org-agenda-start-on-weekday nil
        org-agenda-use-time-grid nil)
  (setq org-agenda-sorting-strategy
        '((agenda time-up priority-down category-keep alpha-up)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))
  (setq org-agenda-custom-commands
        '(("A" tags-todo "PRIORITY=\"A\"" nil)
          ("d" todo "DONE" nil)
          ("i" tags-todo "category=\"inbox\""
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'nottimestamp)))))
          ("u" "Unschedule TODO entries" alltodo ""
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'scheduled 'deadline
                                         'regexp "\n]+>")))
            (org-agenda-overriding-header "Unscheduled TODO entries: ")))
          ("p" "Past timestamps" tags "TIMESTAMP<=\"<now>\"")))

  (advice-add 'org-agenda-goto-today :around
              (lambda (fn &rest args)
                (if (org-agenda-check-type nil 'agenda)
                    (apply fn args)
                  (goto-char (point-min)))))

  (bind-keys :map org-agenda-mode-map
             ;; Bind `org-agenda-follow-mode' to same key as
             ;; `next-error-follow-minor-mode'.
             ("C-c C-f" . org-agenda-follow-mode)
             ("C-o" . org-agenda-show-and-scroll-up)
             ;; Free up 'j' for `km/org-agenda-avy-goto-subword-1'.
             ("C-j" . org-agenda-goto-date)
             ("a" . org-agenda)
             ("d" . org-agenda-view-mode-dispatch)
             ("i" . km/org-agenda-reschedule-by-days)
             ("k" . nil)
             ("v" . org-agenda-capture))

  (require 'km-org-agenda))

(use-package org-contacts
  :init
  (setq org-contacts-files '("~/notes/contacts.org"))
  :config
  (add-to-list 'org-capture-templates
               '("a" "email address" entry
                 (file+headline "~/notes/contacts.org" "Inbox")
                 "
** %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")))

(use-package org-attach
  :init
  (put 'org-attach-directory 'safe-local-variable #'stringp))

(use-package org-board
  :init
  (setq org-board-default-browser 'system)
  (define-key km/org-prefix-map "b" #'org-board-open)
  :config
  (setq org-board-wget-switches
        (delete "-e robots=off" org-board-wget-switches))

  (advice-add 'org-board-archive :before
              (lambda (&rest _)
                (unless (yes-or-no-p "Archive entry? ")
                  (user-error "Archiving aborted")))
              '((name . "org-board-archive-confirm"))))

(use-package org-clock
  :config
  (advice-add
   'org-clock-in :after
   (lambda (&rest _) (org-save-all-org-buffers))
   '((name . "org-clock-in-save-buffers")))

  (advice-add
   'org-clock-out :after
   (lambda (&rest _) (org-save-all-org-buffers))
   '((name . "org-clock-out-save-buffers")))

  (advice-add
   'org-resolve-clocks :after
   (lambda (&rest _) (org-save-all-org-buffers))
   '((name . "org-resolve-clocks-save-buffers"))))

(use-package km-org
  :init
  (bind-keys :map km/org-prefix-map
             ("c" . km/org-clone-and-shift-by-repeater)
             ("d" . km/org-link-dired-jump)
             ("D" . km/org-delete-checked-items)
             ("i" . km/org-refile-list-item)
             ("l" . km/org-remove-title-leader)
             ("n" . km/org-normalize-spaces)
             ("s" . km/org-sort-parent)
             ("w" . km/org-refile-to-other-org-buffer))
  (bind-keys :map km/file-map
             ("a" . km/org-open-annex-file)
             ("o" . km/org-open-file)
             ("p" . km/org-open-file-at-point)
             ("r" . km/org-open-recent-file))

  (define-key ctl-x-4-map "o" #'km/org-switch-to-buffer-other-window)

  (after 'org
    (define-key org-mode-map (kbd "C-c C-x B")
      #'km/org-tree-to-indirect-buffer-current-window)
    (define-key org-mode-map [remap org-tree-to-indirect-buffer]
      #'km/org-tree-to-indirect-buffer)
    (define-key org-mode-map [remap org-refile] #'km/org-refile-dwim)
    (define-key org-mode-map [remap org-goto] #'km/org-goto)

    (add-to-list 'org-speed-commands-user
                 '("m" . (progn (forward-char 1)
                                (call-interactively
                                 #'org-insert-todo-heading-respect-content))))
    (add-to-list 'org-speed-commands-user '("o" . km/org-open-at-point-stay))
    (add-to-list 'org-speed-commands-user '("w" . km/org-refile-dwim))

    (setq org-refile-target-verify-function #'km/org-refile-verify-target)
    (add-hook 'org-after-refile-insert-hook #'km/org-maybe-sort-parent)

    (org-add-link-type "pmid" #'km/org-pmid-open))
  (after 'ox-md
    (advice-add 'org-md-paragraph :filter-return #'km/org-md-fill-string)))

(use-package km-org-agenda
  :init
  (bind-keys :map km/global-org-map
             ("j" . km/org-goto-agenda-heading)
             ("m" . km/org-open-default-notes-file-inbox)
             ("n" . km/org-agenda-add-or-remove-file))
  (after 'org
    (define-key org-mode-map [remap org-agenda-set-restriction-lock]
      #'km/org-agenda-set-restriction-lock))

  :config
  (setq km/org-agenda-file-directory "~/notes/agenda/"
        org-agenda-files (list km/org-agenda-file-directory))

  (add-hook 'org-agenda-finalize-hook #'km/org-agenda-cd-and-read-dir-locals)
  (add-hook 'org-agenda-finalize-hook #'km/org-agenda-store-current-span)

  (add-to-list 'org-agenda-bulk-custom-functions
               '(?D km/org-agenda-delete-subtree))

  (bind-keys :map org-agenda-mode-map
             ("D" . km/org-agenda-delete-subtree)
             ("w" . km/org-agenda-refile-dwim)))

(use-package poporg
  :init
  (define-key km/global-org-map "p" #'poporg-dwim)
  :config
  (add-hook 'poporg-mode-hook #'outline-show-all)
  (define-key poporg-mode-map (kbd "C-c C-c") #'poporg-edit-exit))

(use-package org-link-edit
  :load-path "~/src/emacs/org-link-edit/"
  :init (require 'org-link-edit-autoloads nil t))

(use-package bog
  :load-path "~/src/emacs/bog/"
  :init
  (require 'bog-autoloads nil t)
  (setq bog-keymap-prefix (kbd "C-c b"))
  (global-set-key bog-keymap-prefix bog-command-map)
  :config
  (add-hook 'org-mode-hook #'bog-mode)

  (setq bog-subdirectory-group 2
        bog-combined-bib-ignore-not-found t
        bog-use-citekey-cache t))

(use-package calendar
  :config
  (bind-keys :map calendar-mode-map
             ("C-x [" . calendar-backward-month)
             ("C-x ]" . calendar-forward-month)
             ("M-{" . calendar-backward-year)
             ("M-}" . calendar-forward-year)))


;;; Editing, navigation, and search

(setq set-mark-command-repeat-pop t)
(setq recenter-positions '(top middle bottom))
(setq kill-read-only-ok t)
(setq delete-active-region nil)

(setq history-delete-duplicates t)

(setq-default indicate-empty-lines t
              indent-tabs-mode nil)

(electric-indent-mode -1)
(electric-pair-mode)
(show-paren-mode)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(put 'set-goal-column 'disabled nil)

(bind-keys :map km/editing-map
           ("C-i" . indent-relative)
           ("l" . toggle-truncate-lines))

(bind-keys :map occur-mode-map
           ("n" . next-line)
           ("p" . previous-line))
(bind-keys :map search-map
           ("s" . query-replace)
           ("S" . replace-string)
           ("r" . query-replace-regexp)
           ("R" . replace-regexp))

;; Avoid shift key for `backward-paragraph' and `forward-paragraph'.
(global-set-key (kbd "M-}") nil)
(global-set-key (kbd "M-]") #'forward-paragraph)
(global-set-key (kbd "M-{") nil)
(global-set-key (kbd "M-[") #'backward-paragraph)

;; Swap bindings for scroll-{left,right} and
;; {beginning,end}-of-buffer.
(global-set-key (kbd "M-<") #'scroll-left)
(global-set-key (kbd "C-x <") #'beginning-of-buffer)
(global-set-key (kbd "M->") #'scroll-right)
(global-set-key (kbd "C-x >") #'end-of-buffer)

(global-set-key (kbd "C-z") #'zap-to-char)
(use-package misc
  :bind ("M-z" . zap-up-to-char))

(global-set-key (kbd "C-'") #'backward-kill-word)

(put 'fill-paragraph-function 'safe-local-variable
     (lambda (v) (equal v (lambda (_) t))))

(key-chord-define-global "qp" #'fill-paragraph)

(use-package align
  :bind ("C-x \\" . align-regexp))

(use-package expand-region
  :bind ("C-." . er/expand-region))

(use-package iedit
  :init (define-key km/editing-map "i" #'iedit-mode)
  :config
  (setq iedit-toggle-key-default nil))

(use-package easy-kill
  :init
  (global-set-key [remap kill-ring-save] #'easy-kill))

(use-package whitespace
  :config
  :init
  (put 'whitespace-mode 'safe-local-variable #'booleanp)
  (define-key km/editing-map "t" #'whitespace-mode)

  (setq whitespace-style '(face trailing indentation))

  (defun km/cleanup-buffer ()
    (interactive)
    (unless (or whitespace-mode global-whitespace-mode)
      (whitespace-cleanup)
      (delete-trailing-whitespace)))
  (add-hook 'before-save-hook #'km/cleanup-buffer))

(use-package km-editing
  :chords ("jx" . km/toggle-line-or-region-comment)
  :init
  (define-key search-map "o" #'km/occur)
  (define-key narrow-map "c" #'km/narrow-to-comment-heading)

  (bind-keys :map km/editing-map
             ("f" . km/fill-surrounding-indented)
             ("u" . km/unfill-paragraph))

  (global-set-key [remap count-words-region]
                  #'km/count-words-region)

  (bind-keys :map km/kill-map
             ("." . km/kill-sentence-at-point)
             ("j" . km/join-next-line-with-space)
             ("l" . km/kill-line-at-point)
             ("p" . km/kill-paragraph-at-point)
             ("s" . km/kill-string-at-point)
             ("w" . km/kill-word-at-point)))

(use-package outline
  :diminish outline-mode)

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package narrow-indirect
  :init
  (bind-keys :map ctl-x-4-map
             ("nd" . ni-narrow-to-defun-indirect-other-window)
             ("nn" . ni-narrow-to-region-indirect-other-window)
             ("np" . ni-narrow-to-page-indirect-other-window)))

(use-package avy
  :chords ("jf" . avy-goto-subword-1)
  :init
  (define-key isearch-mode-map (kbd "C-'") #'avy-isearch))

(use-package ace-link
  :init
  (after 'org
    (define-key org-mode-map (kbd "C-c m o") #'ace-link-org))
  (after 'info
    (define-key Info-mode-map "o" #'ace-link-info))
  (after 'compile
    (define-key compilation-mode-map "o" #'ace-link-compilation))
  (after 'help-mode
    (define-key help-mode-map "o" #'ace-link-help))
  (after 'woman
    (define-key woman-mode-map "o" #'ace-link-woman))
  (after 'eww
    (define-key eww-link-keymap "o" #'ace-link-eww)
    (define-key eww-mode-map "o" #'ace-link-eww)))

(use-package km-ace-link
  :init
  (after 'dired
    ;; This overrides the binding for `dired-find-file-other-window'.
    (define-key dired-mode-map "o" #'km/ace-link-dired)
    (define-key dired-mode-map "r" #'dired-find-file-other-window))
  (after 'notmuch
    (define-key notmuch-hello-mode-map "o" #'km/ace-link-widget))
  (after 'gnus-sum
    (define-key gnus-summary-mode-map "o" #'km/ace-link-widget))
  (after 'gnus-art
    (define-key gnus-article-mode-map "o" #'km/ace-link-widget)))

(use-package km-avy
  :init
  (after 'elfeed-search
    (define-key elfeed-search-mode-map "j" #'km/elfeed-avy-goto-subword-1))
  (after 'gnus-sum
    (define-key gnus-summary-mode-map "j"
      #'km/gnus-avy-goto-subword-and-select))
  (after 'grep
    (define-key grep-mode-map "j" #'km/grep-avy-goto-subword-1))
  (after 'org-agenda
    (define-key org-agenda-mode-map "j" #'km/org-agenda-avy-goto-subword-1))
  (after 'magit
    (define-key magit-refs-mode-map "j" #'km/magit-avy-goto-subword-1))
  (after 'magit-log
    (define-key magit-log-mode-map "j" #'km/magit-avy-goto-subword-1)
    (define-key magit-cherry-mode-map "j" #'km/magit-avy-goto-subword-1))
  (after 'notmuch
    (define-key notmuch-common-keymap "j" #'km/notmuch-avy-goto-subword-1))
  (define-key occur-mode-map "j" #'km/occur-avy-goto-subword-1)
  :config
  (after 'avy
    (add-to-list 'avy-dispatch-alist (cons ?w #'km/avy-action-copy-line))))


;;; Buffers and files

(setq require-final-newline t)

;; Disable `suspend-frame' binding.
(global-set-key (kbd "C-x C-z") nil)

(define-key km/editing-map "r" #'revert-buffer)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package autorevert
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-expert t
        ibuffer-restore-window-config-on-quit t
        ibuffer-show-empty-filter-groups nil))

(use-package km-buffers
  :chords ("js" . km/save-buffers)
  :bind ("C-x k" . km/kill-buffer))

(use-package ffap
  :config
  (setq ffap-machine-p-known 'reject))

(use-package tramp
  :config
  (setq tramp-default-method "sshx"))

(use-package recentf
  :config
  (setq recentf-max-saved-items 100
        recentf-save-file "~/.emacs.d/cache/recentf")
  (setq recentf-exclude
        (append (list (rx string-start
                          "/tmp/scratch"
                          (or string-end
                              (and "." (one-or-more not-newline))))
                      "\\`/gnu/store/"
                      ".elfeed/index\\'"
                      "/itsalltext/")
                recentf-exclude))
  (recentf-mode))

(use-package nlines
  :load-path "~/src/emacs/nlines/"
  :init
  (require 'nlines-autoloads nil t)
  (define-key km/file-map "l" #'nlines-run-command))

(use-package cpinfo
  :load-path "~/src/emacs/cpinfo/"
  :init
  (require 'cpinfo-autoloads nil t)
  (after 'dired
    (define-key km/dired-prefix-map "f" #'cpinfo-copy)))

(use-package view
  :diminish (view-mode . "Vw")
  :chords ("hq" . view-mode)
  :init
  (define-key ctl-x-4-map "v" #'view-file-other-window)
  :config
  (add-hook 'view-mode-hook
            (lambda ()
              (when (and view-mode (bound-and-true-p lispy-mode))
                (lispy-mode -1)
                (add-hook 'view-mode-hook
                          (lambda ()
                            (unless view-mode (lispy-mode 1)))
                          nil 'local))))

  (bind-keys :map view-mode-map
             ("l" . recenter-top-bottom)
             ("f" . forward-word)
             ("b" . backward-word)
             ("]" . forward-paragraph)
             ("[" . backward-paragraph)
             ("j" . avy-goto-subword-1)))

(use-package km-files
  :bind (("C-c s" . km/scratch-find-file)
         ("C-x C-w" . km/write-file))
  :init
  (bind-keys :map km/file-map
             ("j" . km/dired-jump-file-at-point)
             ("n" . km/rename-current-buffer-file)
             ("t" . km/touch-buffer-file))
  (bind-keys :map ctl-x-4-map
             ("r" . km/recentf-find-file-other-window)
             ("s" . km/scratch-find-file-other-window)))


;;; Frames and windows

(use-package winner
  :defer 10
  :chords ("lq" . winner-undo)
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (winner-mode))

(use-package km-framewin
  :init
  (define-key ctl-x-4-map "c"
    #'km/clone-indirect-buffer-other-window-and-widen))

(use-package ace-window
  :commands km/ace-window
  :chords ("jw" . km/ace-window)
  :init
  (defun km/ace-window (arg)
    "Run `ace-window', swapping single and double C-u's."
    (interactive "p")
    (cl-case arg
      (4 (setq arg 16))
      (16 (setq arg 4)))
    (ace-window arg))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))


;;; Projectile

(use-package projectile
  :diminish projectile-mode
  :chords ("jq" . projectile-commander)
  :init
  (define-key ctl-x-4-map "p" 'km/projectile-ctl-x-4-map)
  (bind-keys :map km/projectile-ctl-x-4-map
             ("C-o" . projectile-display-buffer)
             ("b" . projectile-switch-to-buffer-other-window)
             ("d" . projectile-find-dir-other-window)
             ("f" . projectile-find-file-other-window)
             ("t" . projectile-find-implementation-or-test-other-window))
  :config
  (setq projectile-find-dir-includes-top-level t
        projectile-completion-system 'helm
        projectile-use-git-grep t)

  (defun km/projectile-ignore-directory-p (name)
    (or (file-remote-p name)
        (string-prefix-p "/tmp/" name)
        (string-prefix-p "/gnu/store/" name)))
  (setq projectile-ignored-project-function #'km/projectile-ignore-directory-p)

  (projectile-register-project-type 'snakemake
                                    '("Snakefile") "snakemake -p" "")

  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)

  (bind-keys :map projectile-command-map
             ("g" . projectile-vc)
             ("i" . projectile-ibuffer)
             ("I" . projectile-invalidate-cache)
             ("l" . projectile-project-buffers-other-buffer)
             ("q" . projectile-replace)
             ("s" . projectile-grep))

  ;; I'm redefining a lot of bindings, so unset pre-defined methods
  ;; and define everyting here.
  (setq projectile-commander-methods nil)

  (def-projectile-commander-method ?c
    "Run project compilation command."
    (call-interactively 'projectile-compile-project))
  (def-projectile-commander-method ?D
    "Find a project directory in other window."
    (call-interactively 'projectile-find-dir-other-window))
  (def-projectile-commander-method ?F
    "Find project file in other window."
    (call-interactively 'projectile-find-file-other-window))
  (def-projectile-commander-method ?g
    "Open project root in vc-dir or magit."
    (projectile-vc))
  (def-projectile-commander-method ?i
    "Open an IBuffer window showing all buffers in the current project."
    (call-interactively 'projectile-ibuffer))
  (def-projectile-commander-method ?o
    "Display a project buffer in other window."
    (call-interactively 'projectile-display-buffer))
  (def-projectile-commander-method ?O
    "Run multi-occur on project buffers."
    (projectile-multi-occur))
  (def-projectile-commander-method ?r
    "Find recently visited file in project."
    (projectile-recentf))
  (def-projectile-commander-method ?s
    "Run grep on project."
    (call-interactively #'projectile-grep))

  (projectile-global-mode)
  (require 'helm-projectile))

(use-package helm-projectile
  :config
  (bind-keys :map projectile-command-map
             ("b" . helm-projectile-switch-to-buffer)
             ("d" . helm-projectile-find-dir)
             ("f" . helm-projectile-find-file)
             ("F" . helm-projectile-find-file-in-known-projects)
             ("j" . helm-etags-select)
             ("p" . helm-projectile-switch-project)
             ("r" . helm-projectile-recentf))
  (def-projectile-commander-method ?b
    "Find project buffer."
    (call-interactively 'helm-projectile-switch-to-buffer))
  (def-projectile-commander-method ?d
    "Find directory in project."
    (helm-projectile-find-dir))
  (def-projectile-commander-method ?f
    "Open project file."
    (helm-projectile-find-file))
  (def-projectile-commander-method ?j
    "Find project tag."
    (helm-etags-select nil))
  (def-projectile-commander-method ?l
      "Run `helm-projectile-grep'."
      (helm-projectile-grep))
  (def-projectile-commander-method ?p
    "Switch project."
    (helm-projectile-switch-project)))

(use-package km-projectile
  :chords ("gp" . km/projectile-switch-project)
  :init
  (define-key km/projectile-ctl-x-4-map "v"
    #'km/projectile-view-file-other-window)

  (after 'projectile
    (bind-keys :map projectile-command-map
               ("4 v" . km/projectile-view-file-other-window)
               ("." . km/projectile-copy-project-filename-as-kill)
               ("e" . km/projectile-restore-thing)
               ("k" . km/projectile-kill-buffers)
               ("K" . km/projectile-kill-nondisplayed-buffers)
               ("v" . km/projectile-view-file)
               ("w" . km/projectile-save-thing))

    (def-projectile-commander-method ?e
      "Restore saved thing."
      (km/projectile-restore-thing))
    (def-projectile-commander-method ?k
      "Kill all project buffers."
      (call-interactively #'km/projectile-kill-buffers))
    (def-projectile-commander-method ?v
      "View project file."
      (km/projectile-view-file))
    (def-projectile-commander-method ?V
      "View project file in other window."
      (km/projectile-view-file-other-window))
    (def-projectile-commander-method ?w
      "Save thing."
      (call-interactively #'km/projectile-save-thing))))


;;; Version control

(use-package vc
  :init
  (setq vc-follow-symlinks t))

(use-package vc-git
  :commands vc-git-grep
  :init
  (setq vc-git-resolve-conflicts nil))

(use-package smerge-mode
  :config
  (setq smerge-diff-switches '("-d" "-b" "-u"))

  (defvar-local km/smerge-restore-flycheck nil)
  (add-hook 'smerge-mode-hook
            (lambda ()
              (if smerge-mode
                  (when (bound-and-true-p flycheck-mode)
                    (setq km/smerge-restore-flycheck t)
                    (flycheck-mode -1))
                (when km/smerge-restore-flycheck
                  (setq km/smerge-restore-flycheck nil)
                  (flycheck-mode 1))))))

(use-package git-annex
  :config
  (setq git-annex-commit nil))

(use-package with-editor
  :load-path "~/src/emacs/with-editor/")

(use-package ghub
  :load-path "~/src/emacs/ghub/")

(use-package magit-popup
  :load-path "~/src/emacs/magit-popup/"
  :config
  (setq magit-popup-show-help-echo nil
        magit-popup-show-common-commands nil
        magit-popup-use-prefix-argument 'default)

  (bind-keys :map magit-popup-mode-map
             ("SPC <t>" . magit-invoke-popup-switch)
             ("SPC SPC <t>" . magit-invoke-popup-option)))

(use-package magit
  :load-path "~/src/emacs/magit/lisp/"
  :bind ("C-x g" . km/magit-status)
  :chords ("jg" . km/magit-status)
  :init
  (load "magit-autoloads.el" t)
  (bind-keys :map km/git-map
             ("d" . magit-dispatch-popup)
             ("g" . magit-file-popup)
             ("l" . magit-log-buffer-file)
             ("s" . magit-stage-file))
  (define-key km/git-map "w" 'km/magit-wip-map)
  (setq magit-push-current-set-remote-if-missing nil)
  (setq magit-log-margin '(nil age magit-log-margin-width t 18))
  :config
  (setq magit-delete-by-moving-to-trash nil
        magit-update-other-window-delay 0.1
        magit-revision-insert-related-refs nil
        magit-use-sticky-arguments 'current
        magit-log-show-refname-after-summary t
        magit-log-section-arguments nil)

  (setq magit-list-refs-sortby "-creatordate")

  (setq magit-log-arguments
        (cons "-n75" (cl-remove-if
                      (lambda (x) (string-prefix-p "-n" x))
                      magit-log-arguments)))

  (setq magit-uniquify-buffer-names nil)

  (setq magit-no-confirm '(stage-all-changes unstage-all-changes reverse))
  (setq magit-published-branches nil)

  (add-to-list 'magit-blame-disable-modes 'lispy-mode)

  (setq magit-branch-arguments
        (delete "--track" magit-branch-arguments))
  (setq magit-branch-popup-show-variables nil)

  (setq magit-patch-arguments '("--output-directory=outgoing/"))

  (setq magit-show-refs-arguments '("--sort=-committerdate"))

  (remove-hook 'magit-refs-sections-hook #'magit-insert-tags)

  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)

  (put 'magit-edit-line-commit 'disabled nil)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)

  (define-key ctl-x-4-map "g" #'magit-find-file-other-window)
  (define-key km/file-map "g" #'magit-find-file)

  (bind-keys :map magit-mode-map
             ("o" . magit-push-popup)
             ;; Remove `magit-add-change-log-entry-other-window',
             ;; which overrides my binding for
             ;; `km/zsh-ansi-term-other-window'.
             ("P" . magit-submodule-popup)
             ("C-x 4 a" . nil))

  (define-key magit-file-section-map (kbd "C-j")
    #'magit-diff-visit-file-worktree)
  (define-key magit-hunk-section-map (kbd "C-j")
    #'magit-diff-visit-file-worktree)

  (define-key magit-process-mode-map (kbd "C-c C-k") #'magit-process-kill)

  (define-key magit-mode-map "." 'km/magit-map)
  (bind-keys :map km/magit-map
             ("." . beginning-of-buffer)
             ("n" . magit-revision-toggle-file-filter)
             ("l" . magit-toggle-buffer-lock))

  (magit-define-popup-action 'magit-diff-popup
    ?e "Edit options" #'magit-diff-refresh-popup)

  (magit-define-popup-option 'magit-diff-popup
    ?d "Diff filter" "--diff-filter=")
  (magit-define-popup-option 'magit-diff-mode-refresh-popup
    ?d "Diff filter" "--diff-filter=")

  (magit-change-popup-key 'magit-stash-popup :action
                          ?Z ?s)

  (magit-define-popup-switch 'magit-log-popup
    ?p "First parent" "--first-parent")
  (magit-define-popup-switch 'magit-log-popup
    ?n "No merges" "--no-merges")

  (magit-define-popup-action 'magit-log-popup
    ?e "Edit options" 'magit-log-refresh-popup)

  (magit-define-popup-action 'magit-log-refresh-popup
    ?m "Modify range" 'km/magit-log-modify-range)

  (magit-change-popup-key 'magit-branch-popup :action
                          ?c ?o)
  (magit-change-popup-key 'magit-branch-popup :action
                          ?C ?S)
  (magit-change-popup-key 'magit-branch-popup :action
                          ?n ?C)
  (magit-change-popup-key 'magit-branch-popup :action
                          ?m ?R)
  (magit-change-popup-key 'magit-branch-popup :action
                          ?s ?v)

  (advice-add
   'magit-generate-buffer-name-default-function
   :around
   (lambda (fn mode &optional value)
     (let ((rev (and (listp value) (car value))))
       (when (and rev (stringp rev)
                  (string-match-p (rx string-start
                                      (= 40 (any "a-z" "0-9"))
                                      string-end)
                                  rev))
         (setcar value (magit-rev-abbrev rev))))
     (funcall fn mode value))
   '((name . "magit-buffer-name-shorten-hash")))

  (advice-add 'magit-git-fetch
              :around
              (lambda (fn &rest args)
                (let ((magit-process-popup-time 0))
                  (apply fn args)))
              '((name . "magit-fetch-process")))

  (advice-add 'magit-merge
              :around
              (lambda (fn rev &optional args &rest other)
                (let ((msg (km/magit-merge-pull-message rev)))
                  (when msg
                    (push (format "-m%s" msg) args)))
                (apply fn rev args other))
              '((name . "magit-merge-check-pull")))

  (require 'km-magit))

(use-package git-rebase
  :init
  (setq git-rebase-show-instructions nil))

(use-package magit-wip
  :diminish magit-wip-after-save-local-mode
  :init
  (bind-keys :map km/magit-wip-map
             ("c" . magit-wip-commit)
             ("f" . magit-wip-commit-buffer-file)
             ("l" . magit-wip-log-current)
             ("o" . magit-wip-log)
             ("s" . magit-wip-after-save-local-mode)
             ("S" . magit-wip-after-save-mode))

  (setq magit-wip-merge-branch t)
  :config
  (magit-wip-after-save-mode 1)

  (magit-define-popup-action 'magit-log-popup
    ?w "Log current WIP" 'magit-wip-log-current)
  (magit-define-popup-action 'magit-log-popup
    ?W "Log other WIP" 'magit-wip-log))

(use-package km-magit
  :init
  (bind-keys :map km/git-map
             ("." . km/magit-show-commit-at-point)
             ("c" . km/magit-copy-commit-summary)
             ("e" . km/magit-commit-extend-with-file)
             ("f" . km/magit-reset-file)
             ("i" . km/magit-insert-staged-file)
             ("n" . km/magit-shorten-hash-at-point)
             ("o" . km/magit-log-occurrence)
             ("p" . km/magit-pin-file)
             ("r" . km/magit-find-recently-changed-file)
             ("t" . km/magit-describe)
             ("u" . km/magit-update-or-auto-commit)
             ("v" . km/magit-revfile-reset))
  :config
  (bind-keys :map magit-mode-map
             ("Q" . km/magit-mode-bury-all-windows)
             ("C-w" . km/magit-copy-as-kill))

  (define-key magit-file-section-map (kbd "C-o")
    #'km/magit-diff-visit-file-other-window)
  (define-key magit-hunk-section-map (kbd "C-o")
    #'km/magit-diff-visit-file-other-window)

  (define-key magit-log-select-mode-map "."
    #'km/magit-log-select-guess-fixup-commit)

  (define-key magit-status-mode-map "jr" #'magit-jump-to-remote-counts)

  (define-key magit-refs-mode-map (kbd "C-c C-t") #'km/magit-refs-toggle-tags)

  (define-key magit-file-section-map [remap magit-visit-thing]
    #'km/magit-diff-visit-file)
  (define-key magit-hunk-section-map [remap magit-visit-thing]
    #'km/magit-diff-visit-file)

  (define-key magit-revision-mode-map (kbd "C-c C-r")
    #'km/magit-revision-insert-related-refs)

  (define-key magit-cherry-mode-map "u"
    #'km/magit-cherry-toggle-upstream-section)

  (bind-keys :map km/magit-map
             ("c" . km/magit-find-commit-file)
             ("g" . km/git-map)
             ("f" . km/magit-flip-revs))

  (define-key km/magit-wip-map "w" #'km/magit-commit-wip-with-file)

  (after 'git-rebase
    (bind-keys :map git-rebase-mode-map
               ("d" . km/git-rebase-fixup-duplicates)
               ("j" . km/git-rebase-join-repeats)
               ("m" . km/git-rebase-move-commit)))

  (setq magit-status-sections-hook
        (append
         (let ((funcs (list #'magit-insert-unpushed-to-upstream-or-recent
                            #'magit-insert-unpushed-to-pushremote
                            #'magit-insert-unpulled-from-pushremote
                            #'magit-insert-unpulled-from-upstream)))
           (cl-remove-if (lambda (x) (memq x funcs))
                         magit-status-sections-hook))
         (list #'km/magit-insert-remote-counts)))

  (magit-define-popup-action 'magit-commit-popup
    ?u "Auto commit" #'km/magit-update-or-auto-commit)

  (magit-define-popup-action 'magit-push-popup
    ?a "Push all" #'km/magit-push-all)
  (magit-define-popup-action 'magit-push-popup
    ?h "Push HEAD" #'km/magit-push-head)
  (magit-change-popup-key 'magit-push-popup :action
                          ?u ?U)
  (magit-change-popup-key 'magit-push-popup :action
                          ?p ?P)

  (magit-define-popup-action 'magit-log-popup
    ?l "Log current" #'km/magit-log-current)
  (magit-define-popup-action 'magit-log-popup
    ?d "Log dwim" #'km/magit-log-dwim)
  (magit-define-popup-action 'magit-log-popup
    ?y "Cherry dwim" #'km/magit-cherry-dwim)

  (magit-define-popup-action 'magit-merge-popup
    ?u "Merge upstream" #'km/magit-ff-merge-upstream)

  (magit-define-popup-action 'magit-branch-popup
    ?c "Create & checkout from current"
    #'km/magit-branch-and-checkout-from-current)

  (magit-define-popup-action 'magit-stash-popup
    ?e "Edit message" #'km/magit-stash-edit-message)

  (magit-define-popup-action 'magit-branch-popup
    ?K "Delete previous branch" #'km/magit-delete-previous-branch)
  (magit-define-popup-action 'magit-branch-popup
    ?m "Checkout master" #'km/magit-checkout-master)
  (magit-define-popup-action 'magit-branch-popup
    ?n "Checkout recent ref" #'km/magit-checkout-recent-ref)
  (magit-define-popup-action 'magit-branch-popup
    ?N "Track recent ref" #'km/magit-checkout-track-recent-ref)
  (magit-define-popup-action 'magit-branch-popup
    ?l "Checkout previous" #'km/magit-checkout-previous-branch)
  (magit-define-popup-action 'magit-branch-popup
    ?r "Rename branch" #'km/magit-branch-rename)
  (magit-define-popup-action 'magit-branch-popup
    ?s "Backup current branch" #'km/magit-branch-backup-current)
  (magit-define-popup-action 'magit-branch-popup
    ?t "Local tracking" #'km/magit-checkout-local-tracking))

(use-package magit-annex
  :load-path "~/src/emacs/magit-annex/"
  :init (require 'magit-annex-autoloads nil t)
  :config
  (setq magit-annex-unused-open-function #'org-open-file))

(use-package magit-tbdiff
  :load-path "~/src/emacs/magit-tbdiff/"
  :init (require 'magit-tbdiff-autoloads nil t))

(use-package magit-imerge
  :load-path "~/src/emacs/magit-imerge/"
  :init (require 'magit-imerge-autoloads nil t))

(use-package git-commit
  ;; :load-path "~/src/emacs/magit/lisp/"
  :config
  (setq git-commit-finish-query-functions nil)

  (add-hook 'git-commit-setup-hook
            (lambda ()
              (add-hook 'with-editor-pre-finish-hook
                        #'git-commit-save-message nil t)))
  (advice-add
   'git-commit-prev-message :after
   (lambda (&rest _) (goto-char (point-min)))
   '((name . "go-to-bob"))))

(use-package orgit
  :disabled t
  :load-path "~/src/emacs/orgit/"
  :init (require 'orgit-autoloads nil t))


;;; Command interfaces

(use-package setkey
  :bind ("C-c v" . setkey-call))

(use-package god-mode
  :bind (("C-c d" . god-local-mode)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right))
  :config
  (add-hook 'view-mode-hook
            (lambda ()
              (if view-mode (god-local-mode-pause) (god-local-mode-resume))))
  (add-hook 'org-capture-mode-hook
            (lambda () (god-local-mode -1)))

  (add-hook 'god-mode-enabled-hook
            (lambda ()
              (when view-mode
                (view-mode -1))
              (when (derived-mode-p 'emacs-lisp-mode)
                (lispy-mode -1))))
  (add-hook 'god-mode-disabled-hook
            (lambda ()
              (when (derived-mode-p 'emacs-lisp-mode)
                (lispy-mode 1))))
  (bind-keys :map god-local-mode-map
             ("." . repeat)
             ("i" . god-local-mode))

  (require 'km-god))

(use-package km-god
  :config
  (add-to-list 'god-exempt-predicates #'km/god-gnus-p)
  (add-hook 'god-mode-enabled-hook #'km/god-update-cursor)
  (add-hook 'god-mode-disabled-hook #'km/god-update-cursor))

(use-package km-hydra
  :bind (("C-c n" . km/hydra-outline-mode)
         ("C-c w" . hydra-window-map/body))
  :commands (hydra-file-search-map/body
             hydra-kmacro/body
             hydra-multiple-cursors/body
             hydra-org-link-edit/body
             hydra-smerge/body)
  :init
  (bind-keys :map km/editing-map
             ("k" . hydra-kmacro/body)
             ("o" . hydra-multiple-cursors/body))
  (define-key km/git-map "m" #'hydra-smerge/body)
  (define-key km/file-map "s" #'hydra-file-search-map/body)
  (after 'org
    (define-key km/org-prefix-map "." #'hydra-org-link-edit/body)))


;;; Helm

(use-package helm
  :config
  (setq helm-move-to-line-cycle-in-source t)

  (require 'km-helm)
  (require 'helm-mode))

(use-package helm-config
  :config
  (global-set-key (kbd "C-x c") nil)
  (customize-set-value 'helm-command-prefix-key "C-c h"))

(use-package helm-buffers
  :chords ("jt" . helm-mini))

(use-package helm-files
  :chords ("jc" . helm-find-files)
  :config
  (setq helm-ff-newfile-prompt-p nil
        helm-ff-file-name-history-use-recentf t
        helm-ff-skip-boring-files t))

(use-package helm-tags
  :config
  (advice-add 'helm-etags-all-tag-files
              :override
              (lambda ()
                (--when-let (helm-etags-get-tag-file)
                  (list it)))
              '((name . "helm-etags-just-project-tags"))))

(use-package km-helm
  :init
  (after 'helm-files
    (bind-keys :map helm-find-files-map
               ("C-c ." . km/helm-visit-in-dired)
               ("C-c b" . km/helm-find-file-below)
               ("C-c x" . km/helm-ff-org-open-file)
               ("C-c C-o" . km/helm-display-file)))
  (after 'helm-locate
    (bind-keys :map helm-generic-files-map
               ("C-c ." . km/helm-visit-in-dired)
               ("C-c b" . km/helm-find-file-below)
               ("C-c x" . km/helm-ff-org-open-file)
               ("C-c C-o" . km/helm-display-file)))
  (after 'helm-buffers
    (bind-keys :map helm-buffer-map
               ("C-c b" . km/helm-display-buffer-below)
               ("C-c C-o" . km/helm-display-buffer)))
  :config
  (after 'elisp-mode
    (add-hook 'emacs-lisp-mode-hook #'km/helm-maybe-override-xref))
  (after 'python
    (add-hook 'python-mode-hook #'km/helm-maybe-override-xref)))

(use-package helm-mode
  :diminish helm-mode
  :config
  (helm-mode 1)

  (add-to-list 'helm-mode-no-completion-in-region-in-modes 'message-mode))

(use-package helm-command
  :chords ("kx" . helm-M-x))

(use-package helm-swoop
  :init
  (setq helm-swoop-pre-input-function (lambda () nil))

  (define-key search-map "k" #'helm-swoop))

(use-package helm-apropos
  :bind ("C-h a" . helm-apropos))

(use-package helm-imenu
  :bind ("C-c l" . helm-imenu))

(use-package helm-man
  :init (define-key km/external-map "m" #'helm-man-woman))

(use-package helm-ring
  :bind (("M-y" . helm-show-kill-ring)
         ("C-x r i" . helm-register))
  :init
  (define-key km/editing-map "m" #'helm-mark-ring))

(use-package helm-bookmark
  :bind ("C-x r b" . helm-bookmarks))

(use-package helm-misc
  :config
  (define-key minibuffer-local-map (kbd "M-r") #'helm-minibuffer-history))


;;; Expansion

(use-package abbrev
  :commands abbrev-mode
  :diminish abbrev-mode
  :init
  (add-hook 'text-mode-hook #'abbrev-mode)
  (add-hook 'prog-mode-hook #'abbrev-mode)
  :config
  (define-abbrev-table 'typo-abbrev-table nil)
  (abbrev-table-put global-abbrev-table :parents
                    (cons typo-abbrev-table
                          (abbrev-table-get global-abbrev-table :parents))))

(use-package km-abbrev
  :init
  (after 'abbrev
    (bind-keys :map abbrev-map
               ("c" . km/abbrev-add-case-global)
               ("iu" . km/abbrev-inverse-add-uppercase-global))))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (defun km/yas-dummy ()
    (interactive)
    (require 'yasnippet)
    (yas-reload-all 'no-jit)
    (message "Yas loaded"))
  (global-set-key (kbd "C-c i") #'km/yas-dummy)
  :config
  (global-set-key (kbd "C-c i") nil)
  (setq yas-fallback-behavior nil)

  (add-to-list 'yas-snippet-dirs
               "~/.guix-profile/share/emacs/yasnippet-snippets/"
               'append)

  (defun km/yas-with-comment (str)
    (concat comment-start
            (unless (s-ends-with? " " comment-start) " ")
            str comment-end))

  (defvar km/personal-snippets
    (file-name-as-directory (expand-file-name "psnippets"
                                              user-emacs-directory)))
  (when (file-exists-p km/personal-snippets)
    (add-to-list 'yas-snippet-dirs km/personal-snippets))

  (bind-keys :map yas-minor-mode-map
             ("C-c i" . yas-expand)
             ;; Remove commands with 'C-c &' prefix, which conflicts
             ;; with `org-mark-ring-goto' binding'
             ("C-c &" . nil)
             ("<tab>" . nil)
             ("TAB" . nil))
  (yas-global-mode))


;;; Dired

(use-package dired
  :config
  (require 'dired-x)
  ;; .git is present as part of `dired-omit-extensions', but this
  ;; seems to only be taken into account if a non-exension part
  ;; exists.
  (setq dired-omit-files
        (rx (or (and bol (zero-or-one ".") "#")
                (and bol "." (zero-or-one ".") eol)
                (and bol ".git" eol)
                (and bol ".gitignore" eol)
                (and bol "__pycache__" eol)
                (and bol ".Rhistory" eol)
                (and bol ".snakemake" eol)
                (and bol ".tramp_history" eol))))

  (setq dired-omit-extensions
        (append dired-omit-extensions
                (list ".aux" ".fdb_latexmk" ".fls"
                      ".log" ".nav" ".out" ".snm"
                      ".vrb")))

  (setq-default dired-omit-mode t)

  (setq dired-dwim-target t)

  (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "zathura")))

  (setq dired-recursive-copies t
        dired-recursive-deletes t)

  (setq dired-deletion-confirmer #'y-or-n-p)

  (put 'dired-find-alternate-file 'disabled nil)

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  (define-key dired-mode-map "c" #'dired-do-copy)

  (define-key dired-mode-map (kbd "C-c m") 'km/dired-prefix-map)

  (require 'git-annex))

(use-package wdired
  :init
  (setq wdired-allow-to-change-permissions t))

(use-package km-dired
  :bind ("C-x C-d" . km/dired-switch-to-buffer)
  :init
  (after 'projectile
    (define-key km/dired-copy-filename-map "p"
      #'km/dired-copy-project-filename-as-kill))
  (define-key km/dired-copy-filename-map "o"
    #'km/dired-copy-relative-filename-as-kill)
  (define-key km/dired-copy-filename-map "w"
    #'dired-copy-filename-as-kill)

  (define-key km/dired-prefix-map "t" #'km/dired-copy-last-mtime-as-kill)

  (after 'dired
    (bind-keys :map dired-mode-map
               ;; This overrides `dired-clean-directory'.
               ("." . km/dired-beginning-of-buffer)
               ;; This overrides `dired-find-file', which is also
               ;; bound to "f".
               ("e" . km/org-open-dired-marked-files)
               ;; This overrides the default binding for
               ;; `dired-copy-filename-as-kill'.
               ("w" . km/dired-copy-filename-map)
               ("C" . km/dired-copy-and-edit)
               ("N" . km/dired-touch-deref)
               ("V" . km/dired-view-file-other-window)))

  (define-key ctl-x-4-map "D" #'km/dired-switch-to-buffer-other-window))

(use-package dired-narrow
  :init
  (after 'dired
    (bind-keys :map km/dired-narrow-prefix-map
               ("f" . dired-narrow-fuzzy)
               ("n" . dired-narrow)
               ("r" . dired-narrow-regexp))
    (define-key km/dired-prefix-map "n" 'km/dired-narrow-prefix-map)
    (define-key dired-mode-map "/" #'dired-narrow)))

(use-package dired-subtree
  :init
  (after 'dired
    (bind-keys :map km/dired-subtree-prefix-map
               ("@" . dired-subtree-mark-subtree)
               ("." . dired-subtree-unmark-subtree)
               ("<" . dired-subtree-beginning)
               (">" . dired-subtree-end)
               ("g" . dired-subtree-revert)
               ("d" . dired-subtree-down)
               ("i" . dired-subtree-insert)
               ("n" . dired-subtree-next-sibling)
               ("p" . dired-subtree-previous-sibling)
               ("r" . dired-subtree-remove)
               ("s" . dired-subtree-narrow)
               ("u" . dired-subtree-up))
    (define-key km/dired-prefix-map "s" 'km/dired-subtree-prefix-map)))


;;; Compilation and shells

(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-c")

(bind-keys :map km/external-map
           ("r" . shell-command-on-region)
           ("s" . shell-command)
           ("S" . shell))

(use-package grep
  :config
  (add-hook 'grep-mode-hook #'toggle-truncate-lines))

(use-package compile
  :init
  (bind-keys :map km/compile-map
             ("c" . compile)
             ("g" . recompile))
  (setq compilation-ask-about-save nil)
  :config
  (advice-add
   'compile :around
   (lambda (f &rest args)
     (if (get-buffer-window (km/compilation-name-by-directory)
                            'visible)
         (save-window-excursion (apply f args))
       (apply f args)))
   '((name . "prevent-duplicate-window")))

  (advice-add
   'recompile :around
   (lambda (f &rest args) (save-window-excursion (apply f args)))
   '((name . "prevent-window")))

  (require 'km-compile))

(use-package km-compile
  :chords ("hv" . km/compilation-recompile)
  :init
  (bind-keys :map km/compile-map
             ("h" . km/compile-in-home-dir)
             ("o" . km/compilation-display-buffer))
  :config
  (setq compilation-buffer-name-function #'km/compilation-name-by-directory))

(use-package km-shell
  :chords ("kz" . km/zsh-toggle-ansi-term-home)
  :init
  (define-key ctl-x-4-map "a" #'km/zsh-ansi-term-other-window)
  (bind-keys :map km/external-map
             ("a" . km/zsh-ansi-term)
             ("t" . km/open-external-terminal)))


;;; Other external programs

(use-package browse-url
  :init
  (define-key km/external-map "b" #'browse-url)
  :config
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"))

(use-package webjump
  :config
  (setq webjump-sites
        '(("DuckDuckGo" .
           [simple-query "https://duckduckgo.com"
                         "https://duckduckgo.com/?q=" ""])
          ("Google Scholar" .
           [simple-query "http://scholar.google.com"
                         "http://scholar.google.com/scholar?&q=" ""])
          ("Dictionary" .
           [simple-query "http://www.dictionary.com/"
                         "http://www.dictionary.com/browse/" "?s=t"])
          ("Wikipedia" .
           [simple-query "wikipedia.org"
                         "wikipedia.org/wiki/" ""])))

  (require 'km-webjump))

(use-package km-webjump
  :init
  (define-key km/external-map "j" #'km/webjump))

(use-package select
  :config
  (setq x-select-enable-clipboard t
        x-select-enable-primary t))

(use-package man
  :config
  (setq Man-notify-method 'aggressive))

(use-package mailcap
  :config
  (mailcap-parse-mailcaps)
  (pcase-dolist (`(_ . ,info)
                 (cdr (assoc-string "application" mailcap-mime-data)))
    ;; Instead of deleting doc-view-mode entry, just make its test
    ;; always fail.
    (when (eq (cdr (assq 'viewer info)) 'doc-view-mode)
      (setf (cdr (assq 'test info)) (lambda (&rest _) nil)))))

(use-package diff
  :init
  (define-key km/external-map "d" 'km/diff-prefix-map)
  (define-key km/diff-prefix-map "d" #'diff)
  :config
  (setq diff-command "/usr/bin/diff"
        diff-switches "-u")

  (require 'diff-mode))

(use-package diff-mode
  :config
  (setq diff-default-read-only t)
  (add-hook 'diff-mode-hook #'toggle-truncate-lines)
  (define-key diff-mode-map (kbd "C-c C-g") #'revert-buffer))

(use-package ediff
  :init
  (define-key km/diff-prefix-map "e" #'ediff)
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package km-diff
  :init
  (bind-keys :map km/diff-prefix-map
             ("c" . km/diff-current-buffer-with-file)
             ("o" . km/diff-with-other-window)
             ("O" . km/ediff-with-other-window))
  (after 'diff-mode
    (define-key diff-mode-map (kbd "C-c C-l") #'km/diff-lock-buffer))
  (after 'ediff
    (add-hook 'ediff-before-setup-hook #'km/ediff-save-window-config)
    (add-hook 'ediff-quit-hook #'km/ediff-restore-window-config)))


;;; Guix

(use-package guix-command
  :init
  (define-key km/external-map "g" #'guix))

(use-package guix-prettify
  :defer 20
  :diminish guix-prettify-mode
  :config
  (global-guix-prettify-mode 1))

(use-package debbugs-gnu
  :config
  (advice-add 'debbugs-gnu-select-report :override
              #'km/debbugs-notmuch-select-report))


;;; Text modes

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(use-package footnote
  :config
  (setq footnote-section-tag ""))

(use-package boxquote
  :init
  (defvar km/boxquote-command-map
    (let ((map (make-sparse-keymap)))
      (define-key map "b" #'boxquote-insert-buffer)
      (define-key map "B" #'boxquote-buffer)
      (define-key map "d" #'boxquote-defun)
      (define-key map "hf" #'boxquote-describe-function)
      (define-key map "hk" #'boxquote-describe-key)
      (define-key map "hv" #'boxquote-describe-variable)
      (define-key map "hw" #'boxquote-where-is)
      (define-key map "f" #'boxquote-insert-file)
      (define-key map "i" #'boxquote-text)
      (define-key map "k" #'boxquote-kill)
      (define-key map "n" #'boxquote-narrow-to-boxquote)
      (define-key map "N" #'boxquote-narrow-to-boxquote-content)
      (define-key map "p" #'boxquote-paragraph)
      (define-key map "r" #'boxquote-region)
      (define-key map "s" #'boxquote-shell-command)
      (define-key map "t" #'boxquote-title)
      (define-key map "u" #'boxquote-unbox)
      (define-key map "U" #'boxquote-unbox-region)
      (define-key map "y" #'boxquote-yank)
      (define-key map (kbd "M-q") #'boxquote-fill-paragraph)
      (define-key map (kbd "M-w") #'boxquote-kill-ring-save)
      map)
    "Keymap for most boxquote commands.")
  (fset 'km/boxquote-command-map km/boxquote-command-map)
  (define-key km/editing-map "b" #'km/boxquote-command-map))

(use-package ispell
  :init
  (define-key km/external-map "i" #'ispell-buffer)
  (define-key km/editing-map "w" #'ispell-word)
  :config
  (setq ispell-program-name "aspell"))

(use-package flyspell
  :diminish (flyspell-mode . "Fy")
  :config
  (setq flyspell-auto-correct-binding (kbd "C-c e ;"))
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package tex-site
  :mode ("\\.[tT]e[xX]\\'" . TeX-latex-mode)
  :config
  (setq font-latex-fontify-sectioning 'color)
  (setq TeX-electric-math '("$" . "$"))

  (after 'latex
    (put 'LaTeX-narrow-to-environment 'disabled nil)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)))

(use-package latex
  :defer t
  :init
  (require 'km-tex))

(use-package km-tex
  :init
  (setq TeX-outline-extra '(("frametitle" 3)))
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq imenu-create-index-function
                    #'km/latex-imenu-create-index-function))))

(use-package reftex
  :diminish (reftex-mode . "Rf")
  :init
  (add-to-list 'safe-local-variable-values
               '(reftex-cite-format . natbib))
  :config
  (add-to-list 'reftex-ref-style-default-list "Cleveref")
  (setq reftex-default-bibliography '("refs.bib")))

(use-package bibtex
  :config
  ;; Make cite key have form <last author last name><year><first word>.
  (setq bibtex-autokey-titlewords 1
        bibtex-autokey-titleword-ignore '("A" "An" "On" "The" "[0-9].*")
        bibtex-autokey-titleword-length nil
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-year-length 4
        bibtex-autokey-year-title-separator "")
  (setq bibtex-align-at-equal-sign t)   ; Used by `bibtex-fill-entry'.
  (setq bibtex-entry-format
        (append '(realign whitespace last-comma delimiters sort-fields)
                bibtex-entry-format))

  (require 'km-bib))

(use-package km-bib
  :config
  (dolist (h '(km/bibtex-delete-article-fields
               km/bibtex-downcase-author-and
               km/bibtex-downcase-entry
               km/bibtex-downcase-keys
               km/bibtex-pages-use-double-hyphen
               km/bibtex-use-title-case
               km/bibtex-remove-doi-leader
               km/bibtex-remove-entry-space
               km/bibtex-set-coding-system
               km/bibtex-single-space-author-list
               km/bibtex-sub-journal))
    (add-hook 'bibtex-clean-entry-hook h)))

(use-package pandoc-mode
  :diminish pandoc-mode)

(use-package writeroom-mode
  :bind ("C-c r" .  writeroom-mode)
  :config
  (setq writeroom-restore-window-config t)
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-width fill-column)

  (defun km/writeroom-toggle-fullscreen ()
    (interactive)
    (setq writeroom-fullscreen-effect
          (if (eq writeroom-fullscreen-effect 'fullboth)
              'maximized
            'fullboth)))

  (defvar-local km/writeroom-old-truncate-lines nil)

  (defun km/writeroom-setup ()
    (if writeroom-mode
        (setq km/writeroom-old-truncate-lines truncate-lines
              truncate-lines nil)
      (setq truncate-lines km/writeroom-old-truncate-lines)))
  (add-hook 'writeroom-mode-hook #'km/writeroom-setup))

(use-package ledger-mode
  :init
  (and (boundp 'font-lock-global-modes)
       (pcase font-lock-global-modes
         ('t
          (setq font-lock-global-modes '(not ledger-mode)))
         (`(not . ,_)
          (add-to-list 'font-lock-global-modes 'ledger-mode t))
         (`(,_ . ,_)
          (setq font-lock-global-modes
                (delq 'ledger-mode font-lock-global-modes))))))


;;; Language modes

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.*rc\\'" . conf-unix-mode))

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(bind-keys :map km/eval-map
           ("b" . eval-buffer)
           ("d" . eval-defun)
           ("e" . eval-expression)
           ("r" . eval-region))

(use-package haskell-mode
  :config
  (setq haskell-process-show-debug-tips nil)
  (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'turn-on-haskell-doc)
  (bind-keys :map haskell-mode-map
             ("C-x C-d" . nil)
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c C-l" . haskell-process-load-file)
             ("C-c C-b" . haskell-interactive-switch)
             ("C-c C-t" . haskell-process-do-type)
             ("C-c C-i" . haskell-process-do-info)
             ("C-c M-." . nil)
             ("C-c C-d" . nil)))

(use-package elisp-mode
  :config
  (defun km/elisp-outline-level ()
    (and (looking-at (concat "^" outline-regexp))
         (- (match-end 0) (match-beginning 0) 3)))

  (defun km/elisp-set-outline-vars ()
    (setq outline-regexp ";;;;* ")
    (setq outline-level #'km/elisp-outline-level))
  (add-hook 'emacs-lisp-mode-hook #'km/elisp-set-outline-vars)

  ;; Modified from usepackage's issue #80.
  (defun km/imenu-add-use-package ()
    (when (string= (buffer-file-name (buffer-base-buffer))
                   user-init-file)
      (add-to-list
       'imenu-generic-expression
       '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2))))
  (add-hook 'emacs-lisp-mode-hook #'km/imenu-add-use-package))

(use-package find-function
  :bind (("C-h ;" . find-function)
         ("C-h 4 ;" . find-function-other-window))
  :init
  (define-key km/file-map "e" #'find-library))

(use-package paredit
  :diminish (paredit-mode . "Pe"))

(use-package lispy
  :init
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook 'scheme-mode-hook #'lispy-mode)
  :config
  (setq lispy-no-permanent-semantic t)

  (define-key lispy-mode-map (kbd "M-.") #'xref-find-definitions)

  (add-hook 'lispy-mode-hook #'km/elisp-set-outline-vars)
  ;; This is ugly, but I haven't found another way to stop
  ;; `imenu-create-index-function' from being set to
  ;; `semantic-create-imenu-index'.  Trying to set it in
  ;; `emacs-lisp-mode-hook' or `lispy-mode-hook' doesn't work.
  (defalias 'semantic-create-imenu-index 'imenu-default-create-index-function))

(use-package geiser
  :config
  (setq geiser-active-implementations '(guile)))

(use-package geiser-mode
  :config
  (define-key geiser-mode-map (kbd "C-.") nil))

(use-package scheme
  :config
  (add-hook 'scheme-mode-hook
            (lambda ()
              (setq imenu-generic-expression
                    (cons (list nil
                                "^(define-public\\s-+\\(\\sw+\\)" 1)
                          imenu-generic-expression)))))

(use-package python
  :init
  (add-to-list 'interpreter-mode-alist '("python2" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python3" . python-mode))

  (pyvenv-tracking-mode 1)
  :config
  (setq python-fill-docstring-style 'pep-257-nn
        python-indent-guess-indent-offset nil)
  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "--simple-prompt --pprint"
        python-shell-completion-native-enable nil
        python-shell-prompt-detect-enabled nil)

  (defun km/python-outline-level ()
    (and (looking-at (concat "^" outline-regexp))
         (- (match-end 0) (match-beginning 0) 3)))

  (defun km/python-set-local-vars ()
    (setq outline-regexp "####* ")
    (setq outline-level #'km/python-outline-level)
    ;; Stop semantic from taking over imenu.
    (setq imenu-create-index-function #'python-imenu-create-index)
    (set (make-local-variable 'compile-command) "pytest-3"))
  (add-hook 'python-mode-hook #'km/python-set-local-vars)
  (add-hook 'python-mode-hook 'flyspell-prog-mode)

  (add-hook 'python-mode-hook
            (lambda ()
              (when (eq major-mode 'python-mode)
                (setq flycheck-checker 'python-flake8)
                (setq-local flycheck-python-flake8-executable "flake8")
                (setq-local flycheck-python-pylint-executable "pylint3")
                (flycheck-mode 1))))

  (bind-keys :map python-mode-map
             ("C-c C-b" . python-shell-send-buffer)
             ("C-c C-f" . python-shell-send-defun)
             ("C-M-x" . python-eldoc-at-point))

  (require 'km-python))

(use-package pydoc
  :load-path "~/src/emacs/pydoc/"
  :config
  (setq pydoc-make-method-buttons nil)
  ;; Don't shadow my `ace-link' binding.
  (define-key pydoc-mode-map "o" #'ace-link-help))

(use-package km-python
  :bind ("C-h y" . km/pydoc)
  :init
  (bind-keys :map km/python-prefix-map
             ("c" . km/python-copy-last-shell-line-as-comment)
             ("t" . km/find-python-test-file-other-window))
  :config
  (bind-keys :map python-mode-map
             ("C-c C-." . km/python-shell-send-buffer-up-to-line)
             ("C-c C-c" . km/python-shell-send-function-or-paragraph-and-step)
             ("C-c C-d" . km/python-shell-send-set-string)
             ("C-c m" . km/python-prefix-map))

  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook
               'post-self-insert-hook
               #'km/python-indent-post-self-insert-function 'append 'local)))

  (add-hook 'pydoc-after-finish-hook #'km/pydoc-store-name)
  (when (file-exists-p km/pydoc-names-file)
    (km/pydoc-read-names-file km/pydoc-names-file)))

(use-package snakemake-mode
  :load-path "~/src/emacs/snakemake-mode/"
  :init
  (require 'snakemake-autoloads nil t)
  (define-key km/compile-map "s" #'snakemake-popup)
  (after 'dired
    (define-key dired-mode-map "b" #'snakemake-popup))
  :config
  (setq snakemake-root-dir-function #'projectile-project-root)

  (defun km/snakemake-set-local-vars ()
    (set (make-local-variable 'imenu-create-index-function)
         #'snakemake-imenu-create-index))
  ;; Although `imenu-create-index-function' is set when snakemake-mode
  ;; is derived from Python mode, I need to define it again here
  ;; because I have a Python mode hook that overrides the Python
  ;; version.
  (add-hook 'snakemake-mode-hook #'km/snakemake-set-local-vars)

  (after 'snakemake
    (magit-change-popup-key 'snakemake-popup :action
                            ?p ?o)))

(use-package km/snakemake
  :init
  (define-key km/compile-map "l" #'km/snakemake-recompile-no-dryrun))

(use-package ess-site
  :mode ("\\.[rR]\\'" . R-mode)
  :config
  (define-key ess-mode-map (kbd "C-c m") 'km/r-prefix-map)
  (define-key km/r-prefix-map "," #'ess-eval-region-or-function-or-paragraph)

  (setq ess-smart-S-assign-key ";")
  (ess-toggle-S-assign nil)
  (ess-toggle-S-assign nil)
  (setq ess-use-ido nil)

  (setq ess-r-package-auto-activate nil
        ess-r-package-auto-set-evaluation-env nil)

  (define-abbrev-table 'ess-mode-abbrev-table
    '(("true" "TRUE")
      ("false" "FALSE"))
    :system t)
  (dolist (hook '(ess-mode-hook inferior-ess-mode-hook))
    (add-hook hook (lambda ()
                     (setq local-abbrev-table ess-mode-abbrev-table)))
    (add-hook hook #'abbrev-mode)))

(use-package km-ess
  :init
  (after 'ess-mode
    (bind-keys :map ess-mode-map
               ("C-c C-." . km/ess-eval-buffer-up-to-line)
               ("|" . km/ess-insert-dplyr-pipe)))
  (after 'ess-inf
    (define-key inferior-ess-mode-map "|" #'km/ess-insert-dplyr-pipe)))

(use-package pkgbuild-mode
  :init
  (setq pkgbuild-update-sums-on-save nil))


;;; Mail

(use-package message
  :init
  (setq message-directory "~/.mail")
  :config
  (load "mail-config.el")
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        message-sendmail-envelope-from 'header
        message-kill-buffer-on-exit t)

  (setq message-make-forward-subject-function #'message-forward-subject-fwd)

  (defun km/message-confirm-sender ()
    "Stop sending messages from the wrong address."
    (unless (y-or-n-p (format "Send message from %s?"
                              (message-field-value "From")))
      (user-error "Not sending message")))
  (add-hook 'message-send-hook #'km/message-confirm-sender)

  (add-hook 'message-mode-hook #'flyspell-mode)
  (add-hook 'message-mode-hook #'whitespace-mode))

(use-package notmuch
  :init
  (autoload 'notmuch "notmuch" "Notmuch mail" t)
  (define-key km/mail-map "n" #'notmuch)
  :config
  (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
  (setq notmuch-archive-tags '("-unread"))
  (setq notmuch-search-oldest-first nil)

  (setq notmuch-show-indent-messages-width 0)

  (setq notmuch-wash-citation-lines-prefix 10)
  (setq notmuch-wash-citation-lines-suffix 10)

  (advice-add
   'notmuch-show-forward-message :around
   (lambda (fn &optional prompt-for-sender)
     (funcall fn (not prompt-for-sender)))
   '((name . "notmuch-show-forward-message--reverse-arg")))

  (advice-add
   'notmuch-show-forward-open-messages :around
   (lambda (fn &optional prompt-for-sender)
     (funcall fn (not prompt-for-sender)))
   '((name . "notmuch-show-forward-open-messages--reverse-arg")))

  (define-key notmuch-show-mode-map "o" #'notmuch-show-reply)
  (define-key notmuch-show-mode-map "O" #'notmuch-show-reply-sender)
  (define-key notmuch-show-mode-map "E" #'notmuch-show-resume-message)

  (define-key notmuch-search-mode-map "o" #'notmuch-search-reply-to-thread)
  (define-key notmuch-search-mode-map "O" #'notmuch-search-reply-to-thread-sender)
  (define-key notmuch-search-mode-map (kbd "C-c C-s") #'notmuch-search-toggle-order)

  (define-key notmuch-tree-mode-map "o"
    (notmuch-tree-close-message-pane-and #'notmuch-show-reply))
  (define-key notmuch-tree-mode-map "O"
    (notmuch-tree-close-message-pane-and #'notmuch-show-reply-sender))
  (define-key notmuch-tree-mode-map "E" #'notmuch-tree-resume-message)

  (define-key notmuch-common-keymap "g" #'notmuch-refresh-this-buffer)

  (define-key notmuch-common-keymap "d" #'notmuch-jump-search)
  (define-key notmuch-message-mode-map (kbd "C-c C-s") nil)
  (define-key notmuch-show-mode-map "v" #'org-capture)

  (define-key notmuch-search-mode-map "." #'beginning-of-buffer)
  (define-key notmuch-tree-mode-map "." #'beginning-of-buffer)

  (define-key notmuch-search-mode-map "r" #'notmuch-search-archive-thread)
  (define-key notmuch-tree-mode-map "r" #'notmuch-tree-archive-message-then-next)

  (define-key notmuch-hello-mode-map "z" nil)
  (define-key notmuch-hello-mode-map "Z" #'notmuch-tree)
  (define-key notmuch-search-mode-map "z"
    #'notmuch-tree-from-search-current-query)
  (define-key notmuch-search-mode-map "Z" #'notmuch-tree)
  (define-key notmuch-show-mode-map "z"
    #'notmuch-tree-from-show-current-query)
  (define-key notmuch-show-mode-map "Z" #'notmuch-tree)
  (define-key notmuch-tree-mode-map "Z" #'notmuch-tree-to-tree)

  (define-key notmuch-show-mode-map "e" #'notmuch-show-open-or-close-all)
  (define-key notmuch-tree-mode-map "e" #'notmuch-tree-show-message)
  (define-key notmuch-search-mode-map "e" #'notmuch-search-show-thread)

  (require 'km-mail))

(use-package km-mail
  :init
  (define-key km/mail-map "." #'km/notmuch-show-at-point)
  :config
  (define-key notmuch-common-keymap [remap notmuch-search]
    #'km/notmuch-search)
  (define-key notmuch-search-mode-map "R" #'km/notmuch-archive-all)
  (define-key notmuch-show-part-map "|"
    #'km/notmuch-show-pipe-part-to-project)
  (define-key notmuch-show-mode-map [remap notmuch-show-pipe-message]
    #'km/notmuch-show-pipe-message-to-project)
  (define-key notmuch-show-stash-map [remap notmuch-show-stash-git-send-email]
    #'km/notmuch-show-stash-git-send-email-debbugs)
  (define-key notmuch-show-mode-map
    [remap notmuch-tree-from-show-current-query]
    #'km/notmuch-tree-from-show-current-query)
  (define-key notmuch-tree-mode-map [remap notmuch-show-pipe-message]
    #'km/notmuch-show-pipe-message-to-project))

(use-package mml
  :diminish (mml-mode . "ML"))

(use-package mm-decode
  :config
  (setq mm-discouraged-alternatives '("text/html" "text/richtext")))

(use-package gnus
  :init
  (bind-keys :map km/mail-map
             ("g" . gnus)
             ("p" . gnus-plugged)
             ("u" . gnus-unplugged))
  (setq gnus-home-directory "~/.gnus.d/"
        gnus-directory gnus-home-directory
        gnus-article-save-directory (expand-file-name "saved/" gnus-directory)
        gnus-kill-files-directory (expand-file-name "scores/" gnus-directory))
  (setq gnus-startup-file (expand-file-name "newsrc" gnus-home-directory)
        gnus-init-file (expand-file-name "gnus" gnus-home-directory)
        gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil
        gnus-inhibit-startup-message t)
  :config
  (setq gnus-summary-line-format "%U%R %&user-date;%-20= %-15,15f  %B %S \n")
  (setq gnus-gcc-mark-as-read t
        gnus-visible-headers '("^From" "^Subject" "^Date" "^To" "^Cc" "^User-Agent")
        gnus-confirm-mail-reply-to-news t)
  (setq gnus-agent-go-online t
        gnus-agent-synchronize-flags t)
  (setq gnus-inhibit-images t)
  (setq gnus-interactive-exit nil)

  (setq gnus-topic-display-empty-topics nil
        gnus-group-line-format "%M\%S\%p\%P\%5y:%B%(%G%)\n"
        gnus-group-list-inactive-groups nil)

  (setq gnus-group-use-permanent-levels t)

  (setq gnus-auto-select-next 'quietly)

  (setq gnus-thread-hide-subtree t)
  (define-key gnus-group-mode-map "e" #'gnus-group-select-group)

  (require 'km-gnus))

(use-package gnus-sum
  :config
  (setq gnus-sum-thread-tree-indent "  "
        gnus-sum-thread-tree-root "."
        gnus-sum-thread-tree-false-root "o "
        gnus-sum-thread-tree-single-indent ""
        gnus-sum-thread-tree-leaf-with-other "+-> "
        gnus-sum-thread-tree-vertical "| "
        gnus-sum-thread-tree-single-leaf "`-> ")
  (bind-keys :map gnus-summary-mode-map
             ("e" . gnus-summary-scroll-up)
             ("v" . org-capture)
             (";" . gnus-summary-universal-argument))
  (define-key gnus-summary-mode-map (kbd "C-c m")
    'km/gnus-summary-prefix-map))

(use-package gnus-art
  :config
  (setq gnus-article-x-face-too-ugly ".*")
  (setq gnus-treat-display-smileys nil)
  (define-key gnus-article-mode-map "v" #'org-capture)
  (define-key gnus-article-mode-map (kbd "C-c m")
    'km/gnus-article-prefix-map))

(use-package shr
  :config
  (bind-keys :map shr-map
             ;; Allow `km/ace-link-widget' binding to work even when
             ;; on shr widget.
             ("o" . nil)
             ("O" . shr-save-contents)
             ("v" . nil)))

(use-package sendmail
  :config
  (setq sendmail-program "/usr/bin/msmtp")
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header))

(use-package km-gnus
  :config

  (add-hook 'kill-emacs-hook #'km/gnus-grace-exit-before-kill-emacs)

  (setq gnus-group-sort-function '(gnus-group-sort-by-alphabet
                                   km/gnus-group-sort-by-topic
                                   gnus-group-sort-by-level))

  (after 'gnus-sum
    (bind-keys :map gnus-summary-mode-map
               ("|" . km/gnus-pipe-to-project)
               ("c" . km/gnus-summary-catchup)
               ("l" . km/gnus-copy-message-link)
               ("o" . km/ace-link-widget)))
  (after 'gnus-art
    (bind-keys :map gnus-article-mode-map
               ("|" . km/gnus-pipe-to-project)
               ("C-c l" . km/gnus-follow-last-message-link)
               ("e" . km/shr-browse-url-and-goto-next)
               ("o" . km/ace-link-widget)))

  (bind-keys :map km/gnus-summary-prefix-map
             ("i" . km/gnus-copy-message-id-as-kill)
             ("l" . km/gnus-copy-gmane-link-as-kill)
             ("p" . km/gnus-open-github-patch))
  (bind-keys :map km/gnus-article-prefix-map
             ("i" . km/gnus-copy-message-id-as-kill)
             ("l" . km/gnus-copy-gmane-link-as-kill)
             ("p" . km/gnus-open-github-patch)))

(use-package elfeed
  :init
  (define-key km/mail-map "e" (lambda ()
                                (interactive)
                                (elfeed)
                                (elfeed-update)))
  :config
  (define-key elfeed-search-mode-map "." #'beginning-of-buffer)
  (define-key elfeed-search-mode-map "e" #'elfeed-search-show-entry)
  (define-key elfeed-show-mode-map "v" #'org-capture)

  (bind-keys :map elfeed-search-mode-map
             ;; Swap search keys.
             ("s" . elfeed-search-set-filter)
             ("S" . elfeed-search-live-filter)))

(use-package km-elfeed
  :init
  (after 'elfeed
    (bind-keys :map elfeed-search-mode-map
               ("c" . km/elfeed-catchup)
               ("d" . km/elfeed-default-filter)
               ("t" . km/elfeed-set-tag))))


;;; Miscellaneous configuration

(key-chord-define-global "cx" "\C-c\C-c")

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t
      enable-recursive-minibuffers t)

(setq bookmark-save-flag nil)

(use-package timer-list
  :init
  (put 'list-timers 'disabled nil))

(use-package savehist
  :config
  (setq savehist-autosave-interval nil))

;; This is intentionally not loaded.
(setq custom-file "~/.emacs.d/.custom.el")

(setq default-input-method "TeX")

(when (file-exists-p (expand-file-name "km-untracked.el" km/init-lisp-dir))
  (require 'km-untracked))

(use-package server
  :config
  (after 'magit-commit
    (remove-hook 'server-switch-hook 'magit-commit-diff))
  (unless (server-running-p)
    (server-start))
  (let ((server (daemonp)))
    (when (string= server "default")
      ;; Remove all mail map bindings except notmuch-related ones.
      (global-set-key (kbd "C-x m n") #'notmuch)
      (global-set-key (kbd "C-x m .") #'km/notmuch-show-at-point)
      (after 'km-python
        (add-hook 'kill-emacs-hook #'km/pydoc-save-names-file))
      (savehist-mode 1)
      (setq save-abbrevs 'silently
            bookmark-save-flag 1)
      (after 'notmuch-lib
        (define-key notmuch-common-keymap "0" #'km/notmuch-sync-mail)))))

;;; init.el ends here
