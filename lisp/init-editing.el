(require-package 'multiple-cursors)
(require-package 'expand-region)
(require-package 'wrap-region)
(require-package 'ace-jump-mode)

(global-set-key (kbd "C-x \\") 'align-regexp)
(key-chord-define-global ";a" 'ace-jump-mode)
(key-chord-define-global ",v" 'view-mode)

(eval-after-load 'view
  '(progn
     (define-key view-mode-map "l" 'recenter-top-bottom)
     (define-key view-mode-map "a" 'ace-jump-mode)
     (diminish 'view-mode "Vw")))

;; Overrides `suspend-emacs' (which is also bound to C-x C-z).
(global-set-key (kbd "C-z") 'zap-to-char)
;; http://irreal.org/blog/?p=1536
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-'") 'backward-kill-word)

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

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

(define-key insert-map "i" 'indent-relative)

;; Put multiple cursors map under insert prefix.
(define-prefix-command 'multiple-cursors-map)
(global-set-key (kbd "C-c c") 'multiple-cursors-map)

(define-key multiple-cursors-map "l" 'mc/edit-lines)
(define-key multiple-cursors-map "b" 'mc/edit-beginnings-of-lines)
(define-key multiple-cursors-map "e" 'mc/edit-ends-of-lines)
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

(defmacro km/make-kill-thing-at-point (name thing kill-func)
  `(defun ,(intern (concat "km/kill-" name "-at-point")) (arg)
     (interactive "p")
     (goto-char (beginning-of-thing ,thing))
     (funcall ,kill-func arg)))

(km/make-kill-thing-at-point "word" 'word 'kill-word)
(km/make-kill-thing-at-point "sentence" 'sentence 'kill-sentence)
(km/make-kill-thing-at-point "paragraph" 'paragraph 'kill-paragraph)
(km/make-kill-thing-at-point "line" 'line 'kill-line)
(km/make-kill-thing-at-point "sexp" 'sexp 'kill-sexp)

(define-key kill-map  "s" 'km/kill-string-at-point)
(define-key kill-map  "." 'km/kill-sentence-at-point)
(define-key kill-map  "w" 'km/kill-word-at-point)
(define-key kill-map  "p" 'km/kill-paragraph-at-point)
(define-key kill-map  "l" 'km/kill-line-at-point)

;; Taken from prelude-core.el.
(defun km/join-next-line-with-space ()
  "Join current line to the next line with a space in between."
  (interactive)
  (delete-indentation 1))

(define-key kill-map  "j" 'km/join-next-line-with-space)

(provide 'init-editing)
