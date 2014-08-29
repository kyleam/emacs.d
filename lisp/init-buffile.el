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

(define-key ctl-x-4-map "v" 'view-file-other-window)

(key-chord-define-global ",s" 'save-buffer)
(key-chord-define-global ",q" 'kill-this-buffer)
(key-chord-define-global ",d" 'km/save-and-kill-buffer)
(key-chord-define-global ",e" '(lambda ()
                                 (interactive)
                                 (save-buffer)
                                 (server-edit)))

(define-prefix-command 'km/file-map)
(global-set-key (kbd "C-c f") 'km/file-map)

(define-key km/file-map "v" 'view-file)

;;; Ibuffer

;; Replace buffer-menu with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq
 ;; Don't prompt to delete unmodified buffers.
 ibuffer-expert t
 ;; Don't show empty filter groups.
 ibuffer-show-empty-filter-groups nil)

;; Recent files
(setq recentf-save-file "~/.emacs.d/cache/recentf"
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;;; Temporary scratch files

(define-prefix-command 'km/scratch-map)
(global-set-key (kbd "C-c s") 'km/scratch-map)

(defmacro km/make-find-scratch-func (name extension)
  `(defun ,(intern (concat "km/find-scratch-" name)) ()
     (interactive)
     (find-file ,(concat "/tmp/scratch" extension))))

(km/make-find-scratch-func "elisp" ".el")
(km/make-find-scratch-func "python" ".py")
(km/make-find-scratch-func "shell" ".sh")
(km/make-find-scratch-func "r" ".r")
(km/make-find-scratch-func "haskell" ".hs")
(km/make-find-scratch-func "org" ".org")
(km/make-find-scratch-func "nomode" "")

(define-key km/scratch-map "e" 'km/find-scratch-elisp)
(define-key km/scratch-map "p" 'km/find-scratch-python)
(define-key km/scratch-map "s" 'km/find-scratch-shell)
(define-key km/scratch-map "r" 'km/find-scratch-r)
(define-key km/scratch-map "h" 'km/find-scratch-haskell)
(define-key km/scratch-map "o" 'km/find-scratch-org)
(define-key km/scratch-map "n" 'km/find-scratch-nomode)

(provide 'init-buffile)
