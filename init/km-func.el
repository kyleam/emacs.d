;; http://whattheemacsd.com/
;; whitespace cleanup

;; buffer-specific prevention modified from
;; http://stackoverflow.com/questions/14913398/
;; in-emacs-how-do-i-save-without-running-save-hooks
(defvar km/prevent-cleanup nil
  "If set, `km/cleanup-buffer' does not perform clean up on save")

(defun km/toggle-prevent-cleanup ()
  "Toggle state of `km/prevent-cleanup'"
  (interactive)
  (let ((state t))
    (when km/prevent-cleanup
        (setq state nil))
    (set (make-local-variable 'km/prevent-cleanup) state)))

(defun km/cleanup-buffer ()
  (interactive)
  (unless km/prevent-cleanup
    (unless (equal major-mode 'makefile-gmake-mode)
      (untabify (point-min) (point-max)))
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8)))
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
(define-key insert-map "s" 'km/shebang)

(defun km/insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(define-key insert-map "d" 'km/insert-date)

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

(defun km/todo-comment ()
  "Add commented TODO"
    (interactive)
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (unless (comment-only-p beg end)
        (beginning-of-line)
        (insert "TODO ")
        (comment-region beg (+ end 5))
        (forward-line))))

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

(define-key kill-map  "s" 'km/kill-string-at-point)
(define-key kill-map  "." 'km/kill-sentence-at-point)
(define-key kill-map  "w" 'km/kill-word-at-point)
(define-key kill-map  "p" 'km/kill-paragraph-at-point)
(define-key kill-map  "l" 'km/kill-line-at-point)

(defun km/join-next-line-with-space ()
  "Join current line to the next line with a space in between"
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (just-one-space))

(define-key kill-map  "j" 'km/join-next-line-with-space)

(defadvice recompile (around restore-windows)
  "Prevent recompiling from spawning new windows"
  (save-window-excursion
    ad-do-it))
(ad-activate 'recompile)

(global-set-key (kbd "C-c g") 'recompile)

;; from prelude
(defun km/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(global-set-key (kbd "C-c s") 'km/swap-windows)

(defvar km/terminal "urxvt")

(defun km/open-external-terminal ()
  (interactive)
  (let ((bufname (get-buffer-create "*External terminal*")))
    (with-current-buffer bufname
      (insert "Starting terminal"))
    (start-process "ext-term" bufname km/terminal)))

(define-key external-map "t" 'km/open-external-terminal)

(defun km/export-wrapped-text (arg)
  "Export the text in current buffer as wrapped text.
This is useful for preparing text in emacs and then exporting to
a wrapped buffer for pasting text (e.g., into a web form).

If region is active, export is restricted to the region. If ARG
is non-nil, the region is copied with `x-select-text'."
  (interactive "P")
  (let ((wrapped-buffer (get-buffer-create "*Wrapped export*"))
        beg end)
    (if (region-active-p)
        (progn (setq beg (region-beginning))
               (setq end (region-end)))
      (setq beg (point-min))
      (setq end (point-max)))
    (copy-to-buffer wrapped-buffer beg end)
    (switch-to-buffer-other-window wrapped-buffer)
    (while (not (eobp))
      (forward-paragraph)
      (forward-line -1)
      (unfill-paragraph)
      (forward-line 1))
    (when arg
      (x-select-text (buffer-substring-no-properties (point-min) (point-max))))))

;; http://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun km/toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))
    (switch-to-buffer nil)))
