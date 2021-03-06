;;; km-editing.el --- Editing-related extensions

;; Copyright (C) 2012-2018 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/emacs.d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'dash)
(require 'outline)
(require 's)
(require 'select)
(require 'thingatpt)
(require 'whitespace)

;; http://www.emacswiki.org/emacs/UnfillParagraph
;;;###autoload
(defun km/unfill-paragraph ()
  "Convert a multi-line paragraph to a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;###autoload
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

;;;###autoload
(defun km/reduce-to-single-spaces ()
  "Reduce consecutive blank lines to a single line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([ \t]*\n\\)\\{3,\\}" nil t)
      (forward-line -1)
      (delete-blank-lines))))

;;;###autoload
(defun km/export-wrapped-text (&optional xselect)
  "Export the text in current buffer as wrapped text.

This is useful for preparing text in emacs and then exporting to
a wrapped buffer for pasting text (e.g., into a web form).

With an active region, restrict export to this region.  If
XSELECT is non-nil, copy the region with `x-select-text'."
  (interactive "P")
  (let ((wrapped-buffer (get-buffer-create "*Wrapped export*")))
    (apply #'copy-to-buffer wrapped-buffer
           (if (use-region-p)
               (list (region-beginning) (region-end))
             (list (point-min) (point-max))))
    (with-current-buffer wrapped-buffer
      (while (not (eobp))
        (forward-paragraph)
        (forward-line -1)
        (km/unfill-paragraph)
        (forward-line 1))
      (when xselect
        (x-select-text
         (buffer-substring-no-properties (point-min) (point-max)))))
    (pop-to-buffer wrapped-buffer)))

;;;###autoload
(defun km/strip-comment-lines (beg end)
  "Export text to a new buffer, removing commented lines.
Any line starting with `comment-start' is considered a commented
line.  Text is limited to BEG and END, as defined by the active
region.  If there is no region, the whole buffer is used."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let ((comment-char (or comment-start
                          (user-error "No comment character is defined")))
        (text (buffer-substring-no-properties beg end)))
    (with-current-buffer (get-buffer-create
                          (concat "*" (buffer-name) " - no comments*"))
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (flush-lines (concat "^" (regexp-quote comment-char) ".*$"))
      (pop-to-buffer (current-buffer)))))

(defun km/delete-comment-lines (orig-buf)
  (let ((comment-char (with-current-buffer orig-buf
                        comment-start)))
    (when comment-char
      (flush-lines (concat "^" (regexp-quote comment-char) ".*$")))))

(defun km/delete-hyphens (orig-buf)
  (save-excursion
    (while (search-forward "-" nil t)
      (delete-char -1))))

(defvar km/count-words-region-filter-functions
  '(km/delete-comment-lines
    km/delete-hyphens)
  "Hooks run by `km/count-words-region-filtered'.
These will be called in a temporary buffer and should delete any
text that should not be considered by `count-words-region'.  They
will be passed the original buffer.  At the start of the function
call, the point will be at the beginning of the buffer, and these
functions should return it there.")

(defvar km/count-words-region-modes '(text-mode latex-mode)
  "Use `km/count-words-region-filter-functions' in modes derived from these.")

;;;###autoload
(defun km/count-words-region (start end &optional arg)
  "Call `count-words-region', possibly filtering input.
Before counting words, `km/count-words-region-filter-functions'
is called.  If the buffer's mode does not derive from
`text-mode', `count-words-region' is called directly."
  (interactive (if current-prefix-arg
                   (list nil nil current-prefix-arg)
                 (list (region-beginning) (region-end) nil)))
  (if (not (apply #'derived-mode-p km/count-words-region-modes))
      (call-interactively #'count-words-region)
    (let ((mode major-mode)
          (buf (current-buffer))
          (text (apply #'buffer-substring-no-properties
                       (if start
                           (list start end)
                         (list (point-min) (point-max))))))
      (with-temp-buffer
        (insert text)
        (funcall mode)
        (goto-char (point-min))
        (run-hook-with-args 'km/count-words-region-filter-functions
                            buf)
        (when start
          (set-mark (point-min))
          (goto-char (point-max)))
        (call-interactively #'count-words-region)))))

;;;###autoload
(defun km/narrow-to-comment-heading ()
  "Narrow to the current comment heading subtree.
Narrow the buffer from the current comment heading to the next
comment heading of the same level or, if not found, to the end of
the buffer.  A comment is considered a heading if it is at the
beginning of the line and if it conists of 3 or more occurences
of `comment-start'. The number of `comment-start' characters is
taken to indicate the level of the heading (with 3 being the top
level)."
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

;;;###autoload
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
;;;###autoload
(defun km/occur ()
  "Call `occur' with active region or symbol at point."
  (interactive)
  (--when-let (if (use-region-p)
                  (buffer-substring-no-properties
                   (region-beginning) (region-end))
                (thing-at-point 'symbol))
    (push it regexp-history))
  (call-interactively 'occur))


;;; Kill map

;;;###autoload
(defun km/kill-string-at-point ()
  (interactive)
  (let ((string-start (nth 8 (syntax-ppss))))
    (goto-char string-start)
    (kill-sexp)))

;; Taken from prelude-core.el.
;;;###autoload
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

(provide 'km-editing)
;;; km-editing.el ends here
