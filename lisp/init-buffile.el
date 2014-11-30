;;; Files and buffers

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward
      require-final-newline t)

(defun km/rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (user-error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (user-error "A buffer named '%s' already exists!" new-name)
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
  (or (buffer-file-name) (user-error "No file is currently being edited"))
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

(define-key ctl-x-4-map "v" 'view-file-other-window)

(key-chord-define-global ",s" 'save-buffer)
(key-chord-define-global ",q" 'kill-this-buffer)
(key-chord-define-global ",d" 'km/save-and-kill-buffer)

(define-prefix-command 'km/file-map)
(global-set-key (kbd "C-c f") 'km/file-map)

(define-key km/file-map "v" 'view-file)

(autoload 'vc-git-grep "vc-git"
  "Run git grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.")

;; http://stackoverflow.com/questions/16122801/
;; remove-header-information-from-rgrep-grep-output-in-emacs
(defun hide-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defadvice grep (after hide-grep-header activate) (hide-grep-header))
(defadvice rgrep (after hide-grep-header activate) (hide-grep-header))
(defadvice lgrep (after hide-grep-hxoeader activate) (hide-grep-header))
(defadvice grep-find (after hide-grep-header activate) (hide-grep-header))
(after 'vc-git
  (defadvice vc-git-grep (after hide-grep-header activate) (hide-grep-header)))

(key-chord-define-global ",z" 'rgrep)

;;; Ibuffer

;; Replace buffer-menu with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq
 ;; Don't prompt to delete unmodified buffers.
 ibuffer-expert t
 ;; Don't show empty filter groups.
 ibuffer-show-empty-filter-groups nil)

;;; Recent files

(setq recentf-save-file "~/.emacs.d/cache/recentf"
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; Modified from prelude
(defun km/recentf-find-file ()
  "Find a file from `recentf-list'."
  (interactive)
  (find-file (km/read-recent-file)))

(defun km/recentf-find-file-other-window ()
  "Find a file from `recentf-list' in other window."
  (interactive)
  (find-file-other-window (km/read-recent-file)))

(defun km/read-recent-file ()
  (ido-completing-read "Choose recent file: " recentf-list nil t))

(key-chord-define-global ",r" 'km/recentf-find-file)
;; This overrides `find-file-read-only-other-window', but
;; `view-file-other-window', which I map to 'v', has the same
;; functionality.
(define-key ctl-x-4-map "r" 'km/recentf-find-file-other-window)

;;; Temporary scratch files

(define-prefix-command 'km/scratch-map)
(global-set-key (kbd "C-c s") 'km/scratch-map)

(define-prefix-command 'km/scratch-other-window-map)
(define-key ctl-x-4-map "s" 'km/scratch-other-window-map)

(defmacro km/make-find-scratch-func (name extension)
  `(defun ,(intern (concat "km/find-scratch-" name)) ()
     (interactive)
     (find-file ,(concat "/tmp/scratch" extension))))

(defmacro km/make-find-scratch-other-window-func (name extension)
  `(defun ,(intern (concat "km/find-scratch-" name "-other-window")) ()
     (interactive)
     (find-file-other-window ,(concat "/tmp/scratch" extension))))

(km/make-find-scratch-func "elisp" ".el")
(km/make-find-scratch-func "python" ".py")
(km/make-find-scratch-func "shell" ".sh")
(km/make-find-scratch-func "r" ".r")
(km/make-find-scratch-func "haskell" ".hs")
(km/make-find-scratch-func "org" ".org")
(km/make-find-scratch-func "markdown" ".md")
(km/make-find-scratch-func "nomode" "")

(km/make-find-scratch-other-window-func "elisp" ".el")
(km/make-find-scratch-other-window-func "python" ".py")
(km/make-find-scratch-other-window-func "shell" ".sh")
(km/make-find-scratch-other-window-func "r" ".r")
(km/make-find-scratch-other-window-func "haskell" ".hs")
(km/make-find-scratch-other-window-func "org" ".org")
(km/make-find-scratch-other-window-func "markdown" ".md")
(km/make-find-scratch-other-window-func "nomode" "")

(define-key km/scratch-map "e" 'km/find-scratch-elisp)
(define-key km/scratch-map "p" 'km/find-scratch-python)
(define-key km/scratch-map "s" 'km/find-scratch-shell)
(define-key km/scratch-map "r" 'km/find-scratch-r)
(define-key km/scratch-map "h" 'km/find-scratch-haskell)
(define-key km/scratch-map "o" 'km/find-scratch-org)
(define-key km/scratch-map "m" 'km/find-scratch-markdown)
(define-key km/scratch-map "n" 'km/find-scratch-nomode)

(define-key km/scratch-other-window-map "e"
  'km/find-scratch-elisp-other-window)
(define-key km/scratch-other-window-map "p"
  'km/find-scratch-python-other-window)
(define-key km/scratch-other-window-map "s"
  'km/find-scratch-shell-other-window)
(define-key km/scratch-other-window-map "r"
  'km/find-scratch-r-other-window)
(define-key km/scratch-other-window-map "h"
  'km/find-scratch-haskell-other-window)
(define-key km/scratch-other-window-map "o"
  'km/find-scratch-org-other-window)
(define-key km/scratch-other-window-map "m"
  'km/find-scratch-markdown-other-window)
(define-key km/scratch-other-window-map "n"
  'km/find-scratch-nomode-other-window)

(provide 'init-buffile)
