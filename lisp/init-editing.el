
;; http://irreal.org/blog/?p=1536
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; This is bound separately in `km/editing-map'.
(setq iedit-toggle-key-default nil)

(setq flyspell-auto-correct-binding (kbd "C-c e ;"))

(after 'flyspell
  (define-key flyspell-mode-map (kbd "C-.") nil))

(put 'fill-paragraph-function 'safe-local-variable
     (lambda (v) (equal v (lambda (_) t))))

;; http://www.emacswiki.org/emacs/UnfillParagraph
(defun km/unfill-paragraph ()
  "Convert a multi-line paragraph to a single line of text."
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

(defun km/narrow-to-comment-heading ()
  "Narrow to the current comment heading subtree.

Narrow the buffer from the current comment heading to the next
comment heading of the same level or, if not found, to the end of
the buffer.

A comment is considered a heading if it is at the beginning of
the line and if it conists of 3 or more occurences of
`comment-start'. The number of `comment-start' characters is
taken to indicate the level of the heading (with 3 being the top
level).

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

(defun km/toggle-line-or-region-comment (beg end)
  "Comment or uncomment the current line or region.
If there is an active region, act on all lines that the region
touches."
  (interactive "*r")
  (unless (use-region-p)
    (setq beg (point)
          end (point)))
  (let ((bol (save-excursion (goto-char beg)
                             (line-beginning-position)))
        (eol (save-excursion (goto-char end)
                             (line-end-position))))
    (unless (eq bol eol)
      (comment-or-uncomment-region bol eol)
      (forward-line))))

;; Modified from http://oremacs.com/2015/01/26/occur-dwim/.
(defun km/occur ()
  "Call `occur' with active region or symbol at point."
  (interactive)
  (--when-let (if (use-region-p)
                  (buffer-substring-no-properties
                   (region-beginning) (region-end))
                (thing-at-point 'symbol))
    (push it regexp-history))
  (call-interactively 'occur))

(defun km/occur-avy-goto-subword-1 ()
  "Like `avy-goto-subword-1', but display occurence."
  (interactive)
  (let (avy-all-windows)
    (call-interactively #'avy-goto-subword-1))
  (occur-mode-display-occurrence))

(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "C-.") 'er/expand-region)

(global-set-key [remap kill-ring-save] 'easy-kill)

;; Overrides `suspend-emacs' (which is also bound to C-x C-z).
(global-set-key (kbd "C-z") 'zap-to-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-'") 'backward-kill-word)

(global-set-key (kbd "M-/") 'hippie-expand)

(key-chord-define-global "jx" 'km/toggle-line-or-region-comment)
(key-chord-define-global "qp" 'fill-paragraph)

(define-key ctl-x-4-map "nd" 'ni-narrow-to-defun-indirect-other-window)
(define-key ctl-x-4-map "nn" 'ni-narrow-to-region-indirect-other-window)
(define-key ctl-x-4-map "np" 'ni-narrow-to-page-indirect-other-window)

(define-key narrow-map "c" 'km/narrow-to-comment-heading)

(define-key occur-mode-map "n" 'next-line)
(define-key occur-mode-map "p" 'previous-line)
(define-key occur-mode-map "j" 'km/occur-avy-goto-subword-1)

;; Override default `occur'.
(define-key search-map "o" 'km/occur)
(define-key search-map "s" 'query-replace)
(define-key search-map "S" 'replace-string)
(define-key search-map "r" 'query-replace-regexp)
(define-key search-map "R" 'replace-regexp)

(define-prefix-command 'km/editing-map)
(global-set-key (kbd "C-c e") 'km/editing-map)

(define-key km/editing-map (kbd "C-i") 'indent-relative)
(define-key km/editing-map "f" 'km/fill-surrounding-indented)
(define-key km/editing-map "i" 'iedit-mode)
(define-key km/editing-map "l" 'toggle-truncate-lines)
(define-key km/editing-map "u" 'km/unfill-paragraph)
(define-key km/editing-map "w" 'ispell-word)

(electric-indent-mode -1)
(electric-pair-mode 1)


;;; Buffer cleanup

(setq whitespace-style '(face trailing indentation))

(global-whitespace-mode 1)

(add-hook 'before-save-hook 'km/cleanup-buffer)

(defvar-local km/prevent-cleanup nil
  "If set, `km/cleanup-buffer' does not perform clean up on save.")

(defun km/toggle-prevent-cleanup ()
  "Toggle state of `km/prevent-cleanup'."
  (interactive)
  (if km/prevent-cleanup
      (progn
        (message "Allowing cleanup on save")
        (kill-local-variable 'whitespace-style)
        (global-whitespace-mode 0)
        (global-whitespace-mode 1))
    (message "Preventing cleanup on save")
    (setq-local whitespace-style
         '(face trailing indentation
           tab-mark space-mark newline-mark))
    (global-whitespace-mode 0)
    (global-whitespace-mode 1))
  (setq km/prevent-cleanup (not km/prevent-cleanup)))

(defun km/cleanup-buffer ()
  (interactive)
  (unless km/prevent-cleanup
    (whitespace-cleanup)
    (delete-trailing-whitespace)))

(define-key km/editing-map "t" 'km/toggle-prevent-cleanup)


;;; Kill map

(defun km/kill-string-at-point ()
  (interactive)
  (let ((string-start (nth 8 (syntax-ppss))))
    (goto-char string-start)
    (kill-sexp)))

;; Taken from prelude-core.el.
(defun km/join-next-line-with-space ()
  "Join current line to the next line with a space in between."
  (interactive)
  (delete-indentation 1))

(defmacro km/make-kill-thing-at-point (thing)
  `(defun ,(intern (concat "km/kill-" thing "-at-point")) ()
     ,(format "Kill %s at point." thing)
     (interactive)
     (goto-char (beginning-of-thing (quote ,(make-symbol thing))))
     (,(intern (concat "kill-" thing)) 1)))

(km/make-kill-thing-at-point "line")
(km/make-kill-thing-at-point "paragraph")
(km/make-kill-thing-at-point "sentence")
(km/make-kill-thing-at-point "word")

(define-prefix-command 'km/kill-map)
(global-set-key (kbd "C-c k") 'km/kill-map)

(define-key km/kill-map  "." 'km/kill-sentence-at-point)
(define-key km/kill-map  "j" 'km/join-next-line-with-space)
(define-key km/kill-map  "l" 'km/kill-line-at-point)
(define-key km/kill-map  "p" 'km/kill-paragraph-at-point)
(define-key km/kill-map  "s" 'km/kill-string-at-point)
(define-key km/kill-map  "w" 'km/kill-word-at-point)


;;; Multiple cursors

(define-key km/editing-map "a" 'mc/mark-all-like-this)
(define-key km/editing-map "b" 'mc/edit-beginnings-of-lines)
(define-key km/editing-map "e" 'mc/edit-ends-of-lines)
(define-key km/editing-map "l" 'mc/edit-lines)
(define-key km/editing-map "n" 'mc/mark-next-like-this)
(define-key km/editing-map "p" 'mc/mark-previous-like-this)

(provide 'init-editing)
