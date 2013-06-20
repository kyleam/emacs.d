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
