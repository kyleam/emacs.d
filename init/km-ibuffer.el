;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; modified from http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
;; and here http://www.elliotglaysher.org/emacs/

(setq ibuffer-saved-filter-groups
      '(("home"
         ("emacs-config" (or (filename . ".emacs.d")
                             (filename . ".el")))
         ("org" (or (mode . org-mode)
                    (name . "^\\*Org Agenda")
                    (name . "^\\*Calendar\\*$")))
         ("web" (or (mode . html-mode)
                    (mode . nxml-mode)
                    (mode . css-mode)))
         ("r" (or (mode . ess-mode)
                  (name . "^\\*R\\*$")))
         ("markup" (or (mode . markdown-mode)
                       (mode . rst-mode)))
         ("lua" (name . ".lua$"))
         ("perl" (name . ".pl$"))
         ("python" (name . ".py$"))
         ("git" (name . "\*git"))
         ("text" (name . ".txt"))
         ("shell" (or (name . ".sh$")
                      (name . ".zsh$")))
         ("latex" (or (mode . latex-mode)
                      (mode . LaTeX-mode)
                      (mode . bibtex-mode)
                      (mode . reftex-mode)))
         ("dirs" (or (mode . dired-mode)
                     (name . "^\\*tramp")))
         ("terminal" (name . "^\\*ansi-term\\*$"))
         ("magit" (or (name . "^\\*magit")))
         ("help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")
                     (name . "^\\*Completions\\*$"))))))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1) ; keep buffer list up-to-date
             (ibuffer-switch-to-saved-filter-groups "home")))

;; do not prompt to delete unmodified buffers
(setq ibuffer-expert t)

;; don't show empty filter groups
(setq ibuffer-show-empty-filter-groups nil)
