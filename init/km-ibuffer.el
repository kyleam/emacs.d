;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; modified from http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
;; and here http://www.elliotglaysher.org/emacs/

(setq ibuffer-saved-filter-groups
      '(("home"
         ("elisp" (mode . emacs-lisp-mode))
         ("org" (or (mode . org-mode)
                    (name . "^\\*Org Agenda\\*$")
                    (name . "^\\*Calendar\\*$")))
         ("web" (or (mode . html-mode)
                    (mode . nxml-mode)
                    (mode . css-mode)))
         ("r" (or (mode . ess-mode)
                  (mode . inferior-ess-mode)
                  (name . "^\\*ESS\\*$")))
         ("markup" (or (mode . markdown-mode)
                       (mode . rst-mode)))
         ("lua" (mode . lua-mode))
         ("perl" (mode . perl-mode))
         ("python" (or (mode . python-mode)
                       (name . "^\\*Python")))
         ("haskell" (mode . haskell-mode))
         ("text" (mode . text-mode))
         ("shell" (mode . sh-mode))
         ("latex" (or (mode . latex-mode)
                      (mode . LaTeX-mode)
                      (mode . bibtex-mode)
                      (mode . reftex-mode)))
         ("dir" (or (mode . dired-mode)
                    (name . "^\\*tramp")))
         ("terminal" (mode . term-mode))
         ("packages" (or (name . "^\\*Compile-Log\\*")
                         (name . "^\\*Packages\\*")))
         ("magit" (name . "^\\*magit"))
         ("emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")))
         ("mail" (or (mode . message-mode)
                     (mode . mail-mode)
                     (mode . gnus-group-mode)
                     (mode . gnus-summary-mode)
                     (mode . gnus-article-mode)
                     (mode . notmuch-search-mode)
                     (mode . notmuch-show-mode)
                     (name . "^\\*Mail sync\\*$")
                     (name . "^\\*Gnus sync\\*$")
                     (name . "^\\.newsrc-dribble")))
         ("help" (or (name . "^\\*Help\\*$")
                     (name . "^\\*Apropos\\*$")
                     (name . "^\\*info\\*%")
                     (name . "^\\*Completions\\*"))))))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1) ; keep buffer list up-to-date
             (ibuffer-switch-to-saved-filter-groups "home")))

;; do not prompt to delete unmodified buffers
(setq ibuffer-expert t)

;; don't show empty filter groups
(setq ibuffer-show-empty-filter-groups nil)
