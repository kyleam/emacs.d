;; http://whattheemacsd.com/
;; whitespace cleanup

(defun km/cleanup-buffer ()
  (interactive)
  (unless (equal major-mode 'makefile-gmake-mode)
    (untabify (point-min) (point-max)))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))
(add-hook 'before-save-hook 'km/cleanup-buffer)

(defun km/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
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

(defun km/shebang (&optional lang)
  (interactive "s\language (default python):")
  (if (= (length lang) 0)
      (setq lang "python"))
  (insert "#!/usr/bin/env " lang "\n"))
(global-set-key (kbd "C-c s") 'km/shebang)

(defun km/insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(global-set-key (kbd "C-c d") 'km/insert-date)

(defun km/start-ess ()
  (interactive)
  (require 'ess-site))

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

(defun km/toggle-line-or-region-comment ()
  "Comment/uncomment the current line or region"
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
          (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end))
    (forward-line))

;; kill functions

(defun km/kill-string-at-point ()
  (interactive)
  (let ((string-start (nth 8 (syntax-ppss))))
    (goto-char string-start)
    (kill-sexp)))

(defun km/kill-thing-at-point (thing killthing killarg)
  "Go to the beginning of THING and call KILLTHING with
KILLARG."
  (goto-char (beginning-of-thing thing))
  (funcall killthing killarg))

(defun km/kill-sentence-at-point (arg)
  (interactive "P")
  (km/kill-thing-at-point 'sentence 'kill-sentence arg))

(defun km/kill-word-at-point (arg)
  (interactive "P")
  (km/kill-thing-at-point 'word 'kill-word arg))

(defun km/kill-paragraph-at-point (arg)
  (interactive "P")
  (km/kill-thing-at-point 'paragraph 'kill-paragraph arg))

(defun km/kill-line-at-point (arg)
  (interactive "P")
  (km/kill-thing-at-point 'line 'kill-line arg))

(defun km/kill-sexp-at-point (arg)
  (interactive "P")
  (km/kill-thing-at-point 'sexp 'kill-sexp arg))

(global-set-key (kbd "C-c k s") 'km/kill-string-at-point)
(global-set-key (kbd "C-c k .") 'km/kill-sentence-at-point)
(global-set-key (kbd "C-c k w") 'km/kill-word-at-point)
(global-set-key (kbd "C-c k p") 'km/kill-paragraph-at-point)
(global-set-key (kbd "C-c k l") 'km/kill-line-at-point)

(defun km/join-next-line-with-space ()
  "Join current line to the next line with a space in between"
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (just-one-space))

(global-set-key (kbd "C-c k j") 'km/join-next-line-with-space)
