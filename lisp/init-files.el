
(require 'uniquify)

(add-to-list 'load-path "~/src/emacs/nlines/")
(require 'nlines-autoloads)

(setq require-final-newline t)

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
  "Automatically edit file with root-privileges."
  (interactive)
  (let ((file (read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(defun km/write-file ()
  "Run `write-file'.
Use the current file name as initial input of prompt."
  (interactive)
  (let* ((init-file (and buffer-file-name
                         (file-name-nondirectory buffer-file-name)))
         (new-file (read-file-name "Write file: " nil nil nil
                                   init-file)))
    (write-file new-file t)))

(global-set-key (kbd "C-x C-w") 'km/write-file)

(define-key ctl-x-4-map "v" 'view-file-other-window)

(define-prefix-command 'km/file-map)
(global-set-key (kbd "C-c f") 'km/file-map)

(define-key km/file-map "R" 'km/find-file-as-root)
(define-key km/file-map "n" 'km/rename-current-buffer-file)
(define-key km/file-map "l" 'nlines-run-command)
(define-key km/file-map "v" 'view-file)


;;; Search

(autoload 'vc-git-grep "vc-git"
  "Run git grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.")

(add-hook 'grep-setup-hook 'km/grep-hide-header)

(defun km/grep-hide-header ()
  (let ((beg (save-excursion (goto-char (point-min))
                             (line-beginning-position 5))))
    (narrow-to-region beg (point-max))))

(define-prefix-command 'km/file-search-map)
(define-key km/file-map "s" 'km/file-search-map)

(define-key km/file-search-map "d" 'find-grep-dired)
(define-key km/file-search-map "D" 'find-dired)
(define-key km/file-search-map "f" 'grep-find)
(define-key km/file-search-map "g" 'lgrep)
(define-key km/file-search-map "G" 'grep)
(define-key km/file-search-map "n" 'find-name-dired)
(define-key km/file-search-map "r" 'rgrep)
(define-key km/file-search-map "v" 'vc-git-grep)
(define-key km/file-search-map "z" 'zrgrep)


;;; Recent files

(setq recentf-max-menu-items 15
      recentf-max-saved-items 200
      recentf-save-file "~/.emacs.d/cache/recentf")
(recentf-mode)

;; Modified from prelude
(defun km/recentf-find-file ()
  "Find a file from `recentf-list'."
  (interactive)
  (find-file (km/read-recent-file)))

(defun km/recentf-find-file-other-window ()
  "Find a file from `recentf-list' in other window."
  (interactive)
  (find-file-other-window (km/read-recent-file)))
;; This overrides `find-file-read-only-other-window', but
;; `view-file-other-window', which I map to 'v', has the same
;; functionality.
(defun km/read-recent-file ()
  (completing-read "Choose recent file: " recentf-list nil t))

(define-key ctl-x-4-map "r" 'km/recentf-find-file-other-window)


;;; Scratch files

(defvar km/find-scratch-buffers
  '((?e ".el"  "Elisp")
    (?p ".py"  "Python")
    (?s ".sh"  "Shell")
    (?r ".r"   "R")
    (?h ".hs"  "Haskell")
    (?o ".org" "Org")
    (?m ".md"  "Markdown")
    (?t ".txt" "Text")
    (?n ""     "No mode"))
  "List of scratch buffers.
Format of each element should be (CHARACTER EXTENSION DOC). DOC
is not required.")

(defun km/scratch-find-file (&optional pwd)
  "Find scratch buffer.

Prompt with characters from `km/find-scratch-buffers' to
determine the extension of the scratch file.

With prefix argument PWD, find the scratch file in
`default-directory' instead of /tmp."
  (interactive "P")
  (switch-to-buffer (km/scratch--find-file-no-select pwd)))

(defun km/scratch-find-file-other-window (&optional pwd)
    "Like `km/find-scratch-file', but open buffer in another window."
  (interactive "P")
  (switch-to-buffer-other-window (km/scratch--find-file-no-select pwd)))

(defun km/scratch--find-file-no-select (pwd)
  (find-file-noselect (km/scratch--get-file-name pwd)))

(defun km/scratch--get-file-name (pwd)
  (let* ((choices (mapcar #'car km/find-scratch-buffers))
         (ch (read-char-choice (concat "[" choices "]") choices))
         (ext (cadr (assq ch km/find-scratch-buffers))))
    (concat (if pwd default-directory "/tmp/") "scratch" ext)))

(global-set-key (kbd "C-c s") 'km/scratch-find-file)
(define-key ctl-x-4-map "s" 'km/scratch-find-file-other-window)

(provide 'init-files)
