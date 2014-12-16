
(global-set-key (kbd "C-x \\") 'align-regexp)

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

(define-key ctl-x-4-map "nd" 'ni-narrow-to-defun-other-window)
(define-key ctl-x-4-map "nn" 'ni-narrow-to-region-other-window)
(define-key ctl-x-4-map "np" 'ni-narrow-to-page-indirect-other-window)

;; http://www.emacswiki.org/emacs/UnfillParagraph
(defun km/unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun km/fill-surrounding-indented ()
  "Fill current line with all surrounding lines of same indentation.
This is like `fill-individual-paragraphs', but 1) it acts only on
a single paragraph at point, not all paragraphs in a region, and
2) it doesn't treat lines with the following structure as a
special case.

     foo>    This line with extra indentation starts
     foo> a paragraph that continues on more lines."
  (interactive)
  (save-excursion
    (let ((orig-point (point))
          (level (current-indentation))
          beg end)
      (beginning-of-line)
      (while (and (not beg) (not (bobp)))
        (forward-line -1)
        (when (or (/= level (current-indentation))
                  (looking-at "^\\s-*$"))
          (forward-line)
          (setq beg (point))))
      (goto-char orig-point)
      (beginning-of-line)
      (while (and (not end) (not (eobp)))
        (forward-line)
        (when (or (/= level (current-indentation))
                  (looking-at "^\\s-*$"))
          (forward-line -1)
          (end-of-line)
          (setq end (point))))
      (fill-region (or beg (point-min)) (or end (point-max))))))

(defun km/reduce-to-single-spaces ()
  "Reduce consecutive blank lines to a single line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([ \t]*\n\\)\\{3,\\}" nil t)
      (forward-line -1)
      (delete-blank-lines))))

(define-key search-map "s" 'query-replace)
(define-key search-map "S" 'replace-string)
(define-key search-map "r" 'query-replace-regexp)
(define-key search-map "R" 'replace-regexp)

(defun km/narrow-to-comment-heading ()
  "Narrow to the current comment heading subtree.

A comment is considered a heading if it is at the beginning of
the line and if it conists of 3 or more occurences of
`comment-start'. The number of `comment-start' characters is
taken to indicate the level of the heading (with 3 being the top
level).

The buffer will be narrowed from the current comment heading to
the next comment heading of the same level or, if not found, to
the end of the buffer.

In the examples below, 'x' indicates the current point and '>>>'
and '<<<' mark the bounds of the narrowed region.

---------------------------------------------------------------
    >>>;;; Level one heading
         x

       ;;;; Level two heading

       <<<
       ;;; Another level one heading
------------------------------eob------------------------------

---------------------------------------------------------------
       ;;; Level one heading

    >>>;;;; Level two heading
           x
       <<<
       ;;;; Another level one heading
------------------------------eob------------------------------

---------------------------------------------------------------
    >>>;;; Level one heading
         x

       ;;;; Level two heading
       <<<
------------------------------eob------------------------------"
  (interactive)
  (unless comment-start
    (user-error "Comment syntax is not defined for current buffer"))
  (unless (= (length comment-start) 1)
    (user-error "Buffer's comment string consists of more than one character"))
  (save-excursion
    (widen)
    (let ((outline-regexp (concat (s-repeat 4 comment-start) "*")))
      (outline-mark-subtree)
      (narrow-to-region (region-beginning) (region-end)))))

(define-key narrow-map "c" 'km/narrow-to-comment-heading)

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

;; Put multiple cursors map under insert prefix.
(define-prefix-command 'km/editing-map)
(global-set-key (kbd "C-c e") 'km/editing-map)

(define-key km/editing-map "l" 'mc/edit-lines)
(define-key km/editing-map "b" 'mc/edit-beginnings-of-lines)
(define-key km/editing-map "e" 'mc/edit-ends-of-lines)
(define-key km/editing-map "n" 'mc/mark-next-like-this)
(define-key km/editing-map "p" 'mc/mark-previous-like-this)
(define-key km/editing-map "a" 'mc/mark-all-like-this)

;; Buffer-specific prevention modified from
;; http://stackoverflow.com/questions/14913398/
;; in-emacs-how-do-i-save-without-running-save-hooks.
(defvar km/prevent-cleanup nil
  "If set, `km/cleanup-buffer' does not perform clean up on save.")

(defun km/toggle-prevent-cleanup ()
  "Toggle state of `km/prevent-cleanup'"
  (interactive)
  (if km/prevent-cleanup
      (message "Allowing cleanup on save ")
    (message "Preventing cleanup on save"))
  (set (make-local-variable 'km/prevent-cleanup) (not km/prevent-cleanup)))

(defun km/cleanup-buffer ()
  (interactive)
  (unless km/prevent-cleanup
    (unless indent-tabs-mode
      (untabify (point-min) (point-max)))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'km/cleanup-buffer)

(define-key km/editing-map "t" 'km/toggle-prevent-cleanup)

(global-set-key (kbd "C-;") 'er/expand-region)

(define-key km/editing-map "i" 'indent-relative)
(define-key km/editing-map "f" 'km/fill-surrounding-indented)

(electric-indent-mode -1)

;; From http://whattheemacsd.com/key-bindings.el-01.html
(defun km/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(global-set-key [remap goto-line] 'km/goto-line-with-feedback)

;; Kill map
(define-prefix-command 'km/kill-map)
(global-set-key (kbd "C-c k") 'km/kill-map)

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

(define-key km/kill-map  "s" 'km/kill-string-at-point)
(define-key km/kill-map  "." 'km/kill-sentence-at-point)
(define-key km/kill-map  "w" 'km/kill-word-at-point)
(define-key km/kill-map  "p" 'km/kill-paragraph-at-point)
(define-key km/kill-map  "l" 'km/kill-line-at-point)

;; Taken from prelude-core.el.
(defun km/join-next-line-with-space ()
  "Join current line to the next line with a space in between."
  (interactive)
  (delete-indentation 1))

(define-key km/kill-map  "j" 'km/join-next-line-with-space)

(provide 'init-editing)
