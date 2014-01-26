;;; Files and buffers

(defun km/rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'km/rename-current-buffer-file)

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
(defun km/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; http://emacs-fu.blogspot.com/2013/03/editing-with-root-privileges-once-more.html
(defun km/find-file-as-root ()
  "`ido-find-file` that automatically edits the file with
root-privileges (using tramp/sudo) if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(global-set-key (kbd "C-x F") 'km/find-file-as-root)

(defun km/save-and-kill-buffer ()
  "Save current buffer and then kill it"
  (interactive)
  (save-buffer)
  (kill-this-buffer))

(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(key-chord-define-global ",f" 'find-file)

(key-chord-define-global ",s" 'save-buffer)
(key-chord-define-global ",q" 'kill-this-buffer)
(key-chord-define-global ",d" 'km/save-and-kill-buffer)
(key-chord-define-global ",e" '(lambda ()
                                 (interactive)
                                 (save-buffer)
                                 (server-edit)))

;;; Ibuffer

;; Replace buffer-menu with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Modified from http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
;; and here http://www.elliotglaysher.org/emacs/.

(setq ibuffer-saved-filter-groups
      '(("home"
         ("elisp" (mode . emacs-lisp-mode))
         ("org" (or (mode . org-mode)
                    (name . "^\\*Org Agenda\\*$")
                    (name . "^\\*Calendar\\*$")))
         ("web" (or (mode . html-mode)
                    (mode . nxml-mode)
                    (mode . css-mode)
                    (mode . less-css-mode)))
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
         ("terminal" (or (mode . term-mode)
                         (name . "^\\*Shell Command Output*")
                         (name . "^\\*External terminal*")))
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
                     (name . "^\\*info\\*$")
                     (name . "^\\*Completions\\*"))))))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1) ; Keep buffer list up-to-date.
             (ibuffer-switch-to-saved-filter-groups "home")))

(setq
 ;; Don't prompt to delete unmodified buffers.
 ibuffer-expert t
 ;; Don't show empty filter groups.
 ibuffer-show-empty-filter-groups nil)

(provide 'init-buffile)
