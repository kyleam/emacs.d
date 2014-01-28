(require-package 'multiple-cursors)
(require-package 'expand-region)
(require-package 'wrap-region)
(require-package 'ace-jump-mode)

(global-set-key (kbd "C-x \\") 'align-regexp)
(key-chord-define-global ";a" 'ace-jump-mode)
(key-chord-define-global ",v" 'view-mode)

;; Overrides `suspend-emacs' (which is also bound to C-x C-z).
(global-set-key (kbd "C-z") 'zap-to-char)
(global-unset-key (kbd "M-z"))

(global-set-key (kbd "C-'") 'backward-kill-word)

;; http://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Buffer-specific prevention modified from
;; http://stackoverflow.com/questions/14913398/
;; in-emacs-how-do-i-save-without-running-save-hooks.
(defvar km/prevent-cleanup nil
  "If set, `km/cleanup-buffer' does not perform clean up on save.")

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

;; Replace map
(define-prefix-command 'replace-map)
(global-set-key (kbd "C-c r") 'replace-map)

(define-key replace-map "s" 'query-replace)
(define-key replace-map "S" 'replace-string)
(define-key replace-map "r" 'query-replace-regexp)
(define-key replace-map "R" 'replace-regexp)

;; Insert map
(define-prefix-command 'insert-map)
(global-set-key (kbd "C-c i") 'insert-map)

(defun km/shebang (&optional lang)
  (interactive "s\language (default python):")
  (if (= (length lang) 0)
      (setq lang "python"))
  (insert "#!/usr/bin/env " lang "\n"))
(define-key insert-map "s" 'km/shebang)

(defun km/insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun km/toggle-line-or-region-comment ()
  "Comment/uncomment the current line or region"
    (interactive)
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (comment-or-uncomment-region beg end))
    (forward-line))

(key-chord-define-global ",c" 'km/toggle-line-or-region-comment)

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

(define-key insert-map "d" 'km/insert-date)
(define-key insert-map "t" 'km/todo-comment)
(define-key insert-map "i" 'indent-relative)

;; Put multiple cursors map under insert prefix.
(define-prefix-command 'multiple-cursors-map)
(define-key insert-map "m" 'multiple-cursors-map)

(define-key multiple-cursors-map "l" 'mc/edit-lines)
(define-key multiple-cursors-map "n" 'mc/mark-next-like-this)
(define-key multiple-cursors-map "p" 'mc/mark-previous-like-this)
(define-key multiple-cursors-map "a" 'mc/mark-all-like-this)

(global-set-key (kbd "C-;") 'er/expand-region)

;; Kill map
(define-prefix-command 'kill-map)
(global-set-key (kbd "C-c k") 'kill-map)

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
  "Join current line to the next line with a space in between."
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (just-one-space))

(define-key kill-map  "j" 'km/join-next-line-with-space)

(provide 'init-editing)