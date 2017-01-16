;;; init.el --- Kyle Meyer's Emacs configuration

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


;; (package-initialize)

(require 'cask)
(cask-initialize)
(require 'pallet)
(pallet-mode 1)

(require 'cl-lib)
(require 'dash)
(require 's)

(require 'bind-key)
(require 'use-package)
(require 'use-package-chords)
(key-chord-mode 1)

(defvar km/init-lisp-dir (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path km/init-lisp-dir)
(add-to-list 'custom-theme-load-path km/init-lisp-dir)

(require 'km-util)
(require 'km-emacs-autoloads nil t)


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
(define-prefix-command 'km/notmuch-show-prefix-map)
(define-prefix-command 'km/org-prefix-map)
(define-prefix-command 'km/projectile-ctl-x-4-map)
(define-prefix-command 'km/python-prefix-map)
(define-prefix-command 'km/gnus-article-prefix-map)


;;; Org

(use-package org
  :load-path "~/src/emacs/org-mode/lisp/"
  :mode ("\\.org.txt\\'" . org-mode)
  :init
  (require 'org-loaddefs)
  (add-to-list 'load-path "~/src/emacs/org-mode/contrib/lisp/" t)
  (add-to-list 'Info-directory-list "~/src/emacs/org-mode/doc/")
  (bind-keys :map km/global-org-map
             ("b" . org-iswitchb)
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
        org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@)"
                                      "|" "DONE(d)" "NA(n@)")))
  (setq org-catch-invisible-edits 'error
        org-special-ctrl-k t
        org-insert-heading-respect-content t
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

  (setq org-structure-template-alist
        (append '(("p" "#+property: ")
                  ("o" "#+options: ")
                  ("d" "#+date: ")
                  ("t" "#+title: ")
                  ("S" "#+setupfile: ?")
                  ("n" "#+name: ")
                  ("w" "#+begin_note\n  ?\n#+end_note")
                  ("C" "#+caption: ")
                  ("b" "#+label: ")
                  ("r" "#+attr_latex: ")
                  ("R" "#+attr_html: "))
                (mapcar (lambda (i) (list (car i) (downcase (cadr i))))
                        org-structure-template-alist)))

  (add-hook 'next-error-hook (lambda ()
                               (when (eq major-mode 'org-mode)
                                 (org-show-context))))

  (advice-add
   'org-eval-in-calendar :around
   (lambda (f form &rest _) (funcall f form 'keepdate))
   '((name . "always-keepdate")))

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

  (define-key km/org-prefix-map "r" #'org-element-cache-reset))

(use-package ox-latex
  :defer t
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
  :defer t
  :diminish (orgtbl-mode . "Ot"))

(use-package org-capture
  :defer t
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
           "* %?%i\n\n%x" :prepend t))))

(use-package org-agenda
  :defer t
  :after org-capture
  :init (define-key km/global-org-map "a" #'org-agenda)
  :config
  (setq org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'only-window)
  (setq org-agenda-dim-blocked-tasks nil
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
          ("u" "Unschedule TODO entries" alltodo ""
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'scheduled 'deadline
                                         'regexp "\n]+>")))
            (org-agenda-overriding-header "Unscheduled TODO entries: ")))
          ("p" "Past timestamps" tags "TIMESTAMP<=\"<now>\"")))

  (bind-keys :map org-agenda-mode-map
             ;; Bind `org-agenda-follow-mode' to same key as
             ;; `next-error-follow-minor-mode'.
             ("C-c C-f" . org-agenda-follow-mode)
             ("C-o" . org-agenda-show-and-scroll-up)
             ;; Free up 'j' for `km/org-agenda-avy-goto-subword-1'.
             ("C-j" . org-agenda-goto-date)
             ("a" . org-agenda)
             ("d" . org-agenda-view-mode-dispatch)
             ("k" . nil)
             ("v" . org-agenda-capture)))

(use-package org-contacts
  :defer t
  :after org-capture
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

(use-package km-org
  :defer t
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
  :defer t
  :after org-agenda
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
  :defer t
  :init
  (define-key km/global-org-map "p" #'poporg-dwim)
  :config
  (add-hook 'poporg-mode-hook #'outline-show-all)
  (define-key poporg-mode-map (kbd "C-c C-c") #'poporg-edit-exit))

(use-package org-link-edit
  :load-path "~/src/emacs/org-link-edit/"
  :defer t
  :init (require 'org-link-edit-autoloads nil t))

(use-package bog
  :load-path "~/src/emacs/bog/"
  :defer t
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
  :defer t
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
  :defer t
  :init (define-key km/editing-map "i" #'iedit-mode)
  :config
  (setq iedit-toggle-key-default nil))

(use-package easy-kill
  :defer t
  :init
  (global-set-key [remap kill-ring-save] #'easy-kill))

(use-package whitespace
  :defer 5
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(face trailing indentation))
  (global-whitespace-mode))

(use-package km-editing
  :defer t
  :chords ("jx" . km/toggle-line-or-region-comment)
  :init
  (define-key search-map "o" #'km/occur)
  (define-key narrow-map "c" #'km/narrow-to-comment-heading)

  (bind-keys :map km/editing-map
             ("f" . km/fill-surrounding-indented)
             ("u" . km/unfill-paragraph))

  (global-set-key [remap count-words-region]
                  #'km/count-words-region)
  :config
  (bind-keys :map km/kill-map
             ("." . km/kill-sentence-at-point)
             ("j" . km/join-next-line-with-space)
             ("l" . km/kill-line-at-point)
             ("p" . km/kill-paragraph-at-point)
             ("s" . km/kill-string-at-point)
             ("w" . km/kill-word-at-point)))

(use-package outline
  :defer t
  :diminish outline-mode)

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package narrow-indirect
  :defer t
  :init
  (bind-keys :map ctl-x-4-map
             ("nd" . ni-narrow-to-defun-indirect-other-window)
             ("nn" . ni-narrow-to-region-indirect-other-window)
             ("np" . ni-narrow-to-page-indirect-other-window)))

(use-package avy
  :defer t
  :chords ("jf" . avy-goto-subword-1)
  :init
  (define-key isearch-mode-map (kbd "C-'") #'avy-isearch))

(use-package ace-link
  :defer t
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
  :defer t
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
  :defer t
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
  (define-key occur-mode-map "j" #'km/occur-avy-goto-subword-1))


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

(use-package km-buffer-cleanup
  :config
  (add-hook 'before-save-hook #'km/cleanup-buffer)
  (define-key km/editing-map "t" #'km/toggle-prevent-cleanup))

(use-package km-buffers
  :defer t
  :chords ("js" . km/save-buffers)
  :bind ("C-x k" . km/kill-buffer))

(use-package ffap
  :defer t
  :config
  (setq ffap-machine-p-known 'reject))

(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "sshx"))

(use-package recentf
  :config
  (setq recentf-max-saved-items 50
        recentf-save-file "~/.emacs.d/cache/recentf")
  (setq recentf-exclude
        (append (list (rx string-start
                          "/tmp/scratch"
                          (or string-end
                              (and "." (one-or-more not-newline))))
                      ".elfeed/index\\'"
                      "/itsalltext/")
                recentf-exclude))
  (recentf-mode))

(use-package nlines
  :load-path "~/src/emacs/nlines/"
  :defer t
  :init
  (require 'nlines-autoloads nil t)
  (define-key km/file-map "l" #'nlines-run-command))

(use-package cpinfo
  :load-path "~/src/emacs/cpinfo/"
  :defer t
  :init
  (require 'cpinfo-autoloads nil t)
  (after 'dired
    (define-key km/dired-prefix-map "f" #'cpinfo-copy)))

(use-package view
  :diminish (view-mode . "Vw")
  :defer t
  :chords ("hq" . view-mode)
  :init
  (define-key ctl-x-4-map "v" #'view-file-other-window)
  :config
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
  :config
  (winner-mode))

(use-package km-framewin
  :defer t
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
  :defer t
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
        (string-prefix-p "/tmp/" name)))
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

  (projectile-global-mode))

(use-package helm-projectile
  :after projectile
  :config
  (bind-keys :map projectile-command-map
             ("b" . helm-projectile-switch-to-buffer)
             ("d" . helm-projectile-find-dir)
             ("f" . helm-projectile-find-file)
             ("F" . helm-projectile-find-file-in-known-projects)
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
  (def-projectile-commander-method ?l
      "Run `helm-projectile-grep'."
      (helm-projectile-grep))
  (def-projectile-commander-method ?p
    "Switch project."
    (helm-projectile-switch-project)))

(use-package km-projectile
  :defer t
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
  :defer t
  :init
  (setq vc-follow-symlinks t))

(use-package vc-git
  :commands vc-git-grep
  :init
  (setq vc-git-resolve-conflicts nil))

(use-package smerge-mode
  :defer t
  :config
  (setq smerge-diff-switches '("-d" "-b" "-u")))

(use-package git-annex
  :defer t
  :after dired
  :config
  (setq git-annex-commit nil))

(use-package with-editor
  :load-path "~/src/emacs/with-editor/"
  :defer t)

(use-package magit-popup
  :load-path "~/src/emacs/magit/lisp/"
  :defer t
  :config
  (setq magit-popup-show-help-echo nil
        magit-popup-show-common-commands nil
        magit-popup-use-prefix-argument 'default)

  (bind-keys :map magit-popup-mode-map
             ("SPC <t>" . magit-invoke-popup-switch)
             ("SPC SPC <t>" . magit-invoke-popup-option)))

(use-package magit
  :load-path "~/src/emacs/magit/lisp/"
  :defer t
  :bind ("C-x g" . km/magit-status)
  :chords ("jg" . km/magit-status)
  :init
  (load "magit-autoloads.el" t)
  (bind-keys :map km/git-map
             ("d" . magit-dispatch-popup)
             ("l" . magit-log-buffer-file)
             ("s" . magit-stage-file))
  (define-key km/git-map "w" 'km/magit-wip-map)
  (setq magit-push-current-set-remote-if-missing nil)
  (setq magit-log-margin '(nil age magit-log-margin-width t 18))
  :config
  (setq magit-push-always-verify nil
        magit-delete-by-moving-to-trash nil
        magit-update-other-window-delay 0.1
        magit-revision-show-gravatars nil
        magit-revision-insert-related-refs nil
        magit-log-show-refname-after-summary t
        magit-log-section-arguments nil)

  (setq magit-log-arguments
        (cons "-n75" (cl-remove-if
                      (lambda (x) (string-prefix-p "-n" x))
                      magit-log-arguments)))

  (setq magit-uniquify-buffer-names nil
        magit-buffer-name-format "*%M%v: %t*")
  (setq magit-no-confirm '(stage-all-changes unstage-all-changes reverse))

  (setq magit-branch-arguments
        (delete "--track" magit-branch-arguments))
  (setq magit-branch-popup-show-variables nil)

  (setq magit-patch-arguments '("--output-directory=outgoing/"))

  (setq magit-show-refs-arguments '("--sort=-committerdate"))

  (setq magit-status-sections-hook
        (let ((funcs (list #'magit-insert-unpulled-from-pushremote
                           #'magit-insert-unpulled-from-upstream)))
          (append (cl-remove-if (lambda (x) (memq x funcs))
                                magit-status-sections-hook)
                  funcs)))

  (remove-hook 'magit-refs-sections-hook #'magit-insert-tags)

  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)

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
             ("l" . magit-toggle-buffer-lock))

  (magit-define-popup-action 'magit-diff-popup
    ?e "Edit options" #'magit-diff-refresh-popup)

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

  (defun km/magit-mode-kill-hidden (mode)
    (let ((buffer (magit-mode-get-buffer mode)))
      (unless (or (not buffer)
                  (get-buffer-window buffer)
                  (derived-mode-p mode))
        (kill-buffer buffer))))

  (advice-add 'magit-log-popup
              :before
              (lambda (&rest _)
                (km/magit-mode-kill-hidden 'magit-log-mode))
              '((name . "magit-log-kill-previous")))
  (advice-add 'magit-diff-popup
              :before
              (lambda (&rest _)
                (km/magit-mode-kill-hidden 'magit-diff-mode))
              '((name . "magit-diff-kill-previous")))

  (advice-add 'magit-git-fetch
              :around
              (lambda (fn &rest args)
                (let ((magit-process-popup-time 0))
                  (apply fn args)))
              '((name . "magit-fetch-process"))))

(use-package git-rebase
  :defer t
  :init
  (setq git-rebase-show-instructions nil))

(use-package magit-wip
  :defer t
  :diminish magit-wip-after-save-local-mode
  :config
  (bind-keys :map km/magit-wip-map
             ("a" . magit-wip-after-apply-mode)
             ("b" . magit-wip-before-change-mode)
             ("c" . magit-wip-commit)
             ("f" . magit-wip-commit-buffer-file)
             ("l" . magit-wip-log-current)
             ("o" . magit-wip-log)
             ("s" . magit-wip-after-save-local-mode)
             ("S" . magit-wip-after-save-mode))

  (magit-define-popup-action 'magit-log-popup
    ?w "Log current WIP" 'magit-wip-log-current)
  (magit-define-popup-action 'magit-log-popup
    ?W "Log other WIP" 'magit-wip-log)

  (magit-wip-after-save-mode 1))

(use-package km-magit
  :defer t
  :after magit
  :init
  (bind-keys :map km/git-map
             ("." . km/magit-show-commit-at-point)
             ("c" . km/magit-copy-commit-summary)
             ("e" . km/magit-commit-extend-with-file)
             ("f" . km/magit-reset-file)
             ("i" . km/magit-insert-staged-file)
             ("n" . km/magit-shorten-hash-at-point)
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

  (magit-define-popup-action 'magit-commit-popup
    ?u "Auto commit" #'km/magit-update-or-auto-commit)

  (magit-define-popup-action 'magit-push-popup
    ?a "Push all" #'km/magit-push-all)
  (magit-define-popup-action 'magit-push-popup
    ?h "Push HEAD" #'km/magit-push-head)
  (magit-change-popup-key 'magit-push-popup :action
                          ?u ?U)

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
    ?s "Backup current branch" #'km/magit-backup-branch)
  (magit-define-popup-action 'magit-branch-popup
    ?t "Local tracking" #'km/magit-checkout-local-tracking))

(use-package magit-annex
  :load-path "~/src/emacs/magit-annex/"
  :defer t
  :init (require 'magit-annex-autoloads nil t)
  :config
  (setq magit-annex-unused-open-function #'org-open-file))

(use-package git-commit
  :load-path "~/src/emacs/magit/lisp/"
  :defer t
  :config
  (setq git-commit-finish-query-functions nil)

  (add-hook 'git-commit-setup-hook
            (lambda ()
              (add-hook 'with-editor-pre-finish-hook
                        #'git-commit-save-message nil t)))
  (add-hook 'git-commit-setup-hook #'km/git-commit-turn-on-flyspell)

  (advice-add
   'git-commit-prev-message :after
   (lambda (&rest _) (goto-char (point-min)))
   '((name . "go-to-bob"))))

(use-package orgit
  :disabled t
  :load-path "~/src/emacs/orgit/"
  :defer t
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
             ("i" . god-local-mode)))

(use-package km-god
  :defer t
  :after god-mode
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
  :defer t
  :config
  (setq helm-move-to-line-cycle-in-source t))

(use-package helm-config
  :defer t
  :config
  (global-set-key (kbd "C-x c") nil)
  (customize-set-value 'helm-command-prefix-key "C-c h"))

(use-package helm-buffers
  :defer t
  :chords ("jt" . helm-mini))

(use-package helm-files
  :defer t
  :chords ("jc" . helm-find-files)
  :config
  (setq helm-ff-newfile-prompt-p nil
        helm-ff-file-name-history-use-recentf t
        helm-ff-skip-boring-files t))

(use-package km-helm
  :defer t
  :after helm
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
               ("C-c C-o" . km/helm-display-buffer))))

(use-package helm-mode
  :diminish helm-mode
  :after helm
  :config
  (helm-mode 1)

  (add-to-list 'helm-mode-no-completion-in-region-in-modes 'message-mode))

(use-package helm-command
  :defer t
  :chords ("kx" . helm-M-x))

(use-package helm-swoop
  :defer t
  :init (define-key search-map "k" #'helm-swoop))

(use-package helm-apropos
  :bind ("C-h a" . helm-apropos))

(use-package helm-imenu
  :bind ("C-c l" . helm-imenu))

(use-package helm-man
  :defer t
  :init (define-key km/external-map "m" #'helm-man-woman))

(use-package helm-ring
  :bind (("M-y" . helm-show-kill-ring)
         ("C-x r i" . helm-register))
  :init
  (define-key km/editing-map "m" #'helm-mark-ring))

(use-package helm-bookmark
  :bind ("C-x r b" . helm-bookmarks))


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
  :defer t
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
  :defer t
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
  :defer t
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

  (define-key dired-mode-map (kbd "C-c m") 'km/dired-prefix-map))

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
  :defer t
  :init
  (after 'dired
    (bind-keys :map km/dired-narrow-prefix-map
               ("f" . dired-narrow-fuzzy)
               ("n" . dired-narrow)
               ("r" . dired-narrow-regexp))
    (define-key km/dired-prefix-map "n" 'km/dired-narrow-prefix-map)
    (define-key dired-mode-map "/" #'dired-narrow)))

(use-package dired-subtree
  :defer t
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
  :defer t
  :config
  (add-hook 'grep-mode-hook #'toggle-truncate-lines))

(use-package compile
  :defer t
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
   '((name . "prevent-window"))))

(use-package km-compile
  :after compile
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
  :defer t
  :init
  (define-key km/external-map "b" #'browse-url)
  :config
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"))

(use-package webjump
  :defer t
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
                         "wikipedia.org/wiki/" ""]))))

(use-package km-webjump
  :after webjump
  :defer t
  :init
  (define-key km/external-map "j" #'km/webjump))

(use-package select
  :defer t
  :config
  (setq x-select-enable-clipboard t
        x-select-enable-primary t))

(use-package man
  :defer t
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
  :defer t
  :init
  (define-key km/external-map "d" 'km/diff-prefix-map)
  (define-key km/diff-prefix-map "d" #'diff)
  :config
  (setq diff-command "/bin/diff"
        diff-switches "-u"))

(use-package diff-mode
  :after diff
  :config
  (setq diff-default-read-only t)
  (add-hook 'diff-mode-hook #'toggle-truncate-lines)
  (define-key diff-mode-map (kbd "C-c C-g") #'revert-buffer))

(use-package ediff
  :defer t
  :init
  (define-key km/diff-prefix-map "e" #'ediff)
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package km-diff
  :defer t
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


;;; Text modes

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(use-package footnote
  :defer t
  :config
  (setq footnote-section-tag ""))

(use-package boxquote
  :defer t
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
  :defer t
  :init
  (define-key km/external-map "i" #'ispell-buffer)
  (define-key km/editing-map "w" #'ispell-word)
  :config
  (setq ispell-program-name "aspell"))

(use-package flyspell
  :defer t
  :diminish (flyspell-mode . "Fy")
  :config
  (setq flyspell-auto-correct-binding (kbd "C-c e ;"))
  (define-key flyspell-mode-map (kbd "C-.") nil))

(use-package tex-site
  :mode ("\\.[tT]e[xX]\\'" . TeX-latex-mode)
  :config
  (setq font-latex-fontify-sectioning 'color)
  (setq TeX-electric-math '("$" . "$"))

  (after 'latex
    (put 'LaTeX-narrow-to-environment 'disabled nil)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)))

(use-package km-tex
  :defer t
  :after latex
  :init
  (setq TeX-outline-extra '(("frametitle" 3)))
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq imenu-create-index-function
                    #'km/latex-imenu-create-index-function))))

(use-package reftex
  :diminish (reftex-mode . "Rf")
  :defer t
  :init
  (add-to-list 'safe-local-variable-values
               '(reftex-cite-format . natbib))
  :config
  (add-to-list 'reftex-ref-style-default-list "Cleveref")
  (setq reftex-default-bibliography '("refs.bib")))

(use-package bibtex
  :defer t
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
                bibtex-entry-format)))

(use-package km-bib
  :after bibtex
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
  :defer t
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


;;; Language modes

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.*rc\\'" . conf-unix-mode))

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(global-set-key (kbd "C-c x") #'eval-expression)

(use-package haskell-mode
  :defer t
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
  :defer t
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
  :defer t
  :diminish (paredit-mode . "Pe"))

(use-package lispy
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  :config
  (setq lispy-no-permanent-semantic t)
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
  :defer t
  :config
  (define-key geiser-mode-map (kbd "C-.") nil))

(use-package python
  :defer t
  :init
  (add-to-list 'interpreter-mode-alist '("python2" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python3" . python-mode))
  :config
  (setq python-fill-docstring-style 'pep-257-nn
        python-indent-guess-indent-offset nil)
  (setq python-shell-interpreter "ipython"
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
    (set (make-local-variable 'compile-command) "py.test"))
  (add-hook 'python-mode-hook #'km/python-set-local-vars)

  (bind-keys :map python-mode-map
             ("C-c C-b" . python-shell-send-buffer)
             ("C-c C-f" . python-shell-send-defun)
             ("C-M-x" . python-eldoc-at-point)))

(use-package pydoc
  :load-path "~/src/emacs/pydoc/"
  :defer t
  :config
  (setq pydoc-make-method-buttons nil)
  ;; Don't shadow my `ace-link' binding.
  (define-key pydoc-mode-map "o" #'ace-link-help))

(use-package km-python
  :after python
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
  :defer t
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
  :defer t
  :init
  (define-key km/compile-map "l" #'km/snakemake-recompile-no-dryrun))

(use-package ess-site
  :mode ("\\.[rR]\\'" . R-mode)
  :config
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
  :defer t
  :init
  (after 'ess-mode
    (bind-keys :map ess-mode-map
               ("C-c C-." . km/ess-eval-buffer-up-to-line)
               ("|" . km/ess-insert-dplyr-pipe)))
  (after 'ess-inf
    (define-key inferior-ess-mode-map "|" #'km/ess-insert-dplyr-pipe)))

(use-package pkgbuild-mode
  :defer t
  :init
  (setq pkgbuild-update-sums-on-save nil))


;;; Mail

(use-package notmuch
  :defer t
  :init (define-key km/mail-map "n" #'notmuch)
  :config
  (setq notmuch-fcc-dirs nil
        notmuch-search-oldest-first nil)
  (add-to-list 'notmuch-saved-searches
               '(:name "today" :query "date:today.." :key "."))
  (define-key notmuch-show-mode-map (kbd "C-c m") 'km/notmuch-show-prefix-map)

  (define-key km/notmuch-show-prefix-map "i"
    #'km/notmuch-show-copy-message-id-as-kill))

(use-package message
  :defer t
  :config
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        message-sendmail-envelope-from 'header
        message-kill-buffer-on-exit t)
  (add-hook 'message-mode-hook #'flyspell-mode))

(use-package mml
  :defer t
  :diminish (mml-mode . "ML"))

(use-package mm-decode
  :defer t
  :config
  (setq mm-discouraged-alternatives '("text/html" "text/richtext")))

(use-package gnus
  :defer t
  :init
  (bind-keys :map km/mail-map
             ("g" . gnus)
             ("p" . gnus-plugged)
             ("u" . gnus-unplugged))
  :config
  (setq gnus-home-directory "~/.gnus.d/"
        gnus-directory gnus-home-directory
        gnus-article-save-directory (expand-file-name "saved/" gnus-directory)
        gnus-kill-files-directory (expand-file-name "scores/" gnus-directory))
  (setq gnus-startup-file (expand-file-name "newsrc" gnus-home-directory)
        gnus-init-file (expand-file-name "gnus" gnus-home-directory)
        gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil
        gnus-inhibit-startup-message t)
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
  (define-key gnus-group-mode-map "e" #'gnus-group-select-group))

(use-package gnus-sum
  :defer t
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
  :defer t
  :config
  (setq gnus-article-x-face-too-ugly ".*")
  (setq gnus-treat-display-smileys nil)
  (define-key gnus-article-mode-map "v" #'org-capture)
  (define-key gnus-article-mode-map (kbd "C-c m")
    'km/gnus-article-prefix-map))

(use-package shr
  :defer t
  :config
  (bind-keys :map shr-map
             ;; Allow `km/ace-link-widget' binding to work even when
             ;; on shr widget.
             ("o" . nil)
             ("O" . shr-save-contents)
             ("v" . nil)))

(use-package sendmail
  :defer t
  :config
  (setq sendmail-program "/usr/bin/msmtp"))

(use-package km-mail
  :defer t
  :after gnus
  :config
  (add-hook 'message-send-hook #'km/message-confirm-sender)
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
             ("l" . km/gnus-copy-gmane-link-as-kill)
             ("p" . km/gnus-open-github-patch))
  (bind-keys :map km/gnus-article-prefix-map
             ("l" . km/gnus-copy-gmane-link-as-kill)
             ("p" . km/gnus-open-github-patch)))

(use-package elfeed
  :defer t
  :init
  (define-key km/mail-map "e" (lambda ()
                                (interactive)
                                (elfeed)
                                (elfeed-update)))
  :config
  (define-key elfeed-show-mode-map "v" #'org-capture)

  (bind-keys :map elfeed-search-mode-map
             ;; Swap search keys.
             ("s" . elfeed-search-set-filter)
             ("S" . elfeed-search-live-filter)))

(use-package km-elfeed
  :defer t
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

;; This is intentionally not loaded.
(setq custom-file "~/.emacs.d/.custom.el")

(setq default-input-method "TeX")

(when (file-exists-p (expand-file-name "km-untracked.el" km/init-lisp-dir))
  (require 'km-untracked))

(use-package server
  :config
  (setq server-use-tcp t)
  (unless (server-running-p)
    (server-start))
  (let ((server (daemonp)))
    (cond
     ((string= server "default")
      ;; Remove all mail map bindings except notmuch.
      (global-set-key (kbd "C-x m") nil)
      (global-set-key (kbd "C-x m n") #'notmuch)
      (after 'km-python
        (add-hook 'kill-emacs-hook #'km/pydoc-save-names-file))
      (setq save-abbrevs 'silently
            bookmark-save-flag 1))
     ((string= server "mail")
      (setq mode-line-misc-info
            (cons (propertize " [Mail] " 'face 'font-lock-doc-face)
                  mode-line-misc-info))
      (key-chord-define-global "jg" 'km/mail-map)
      (setq recentf-save-file "~/.emacs.d/cache/recentf-mail")
      (setq save-abbrevs nil)))))

;;; init.el ends here
