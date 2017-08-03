;;; km-diff.el --- Diff-related extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2016 Kyle Meyer <kyle@kyleam.com>

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
(require 'diff)
(require 'ediff)

;;;###autoload
(defun km/diff-lock-buffer ()
  "Rename current diff buffer to include new file name."
  (interactive)
  (rename-buffer
   (format "*Diff: %s*"
           (abbreviate-file-name
            (substring-no-properties (diff-find-file-name))))))

;;;###autoload
(defun km/diff-current-buffer-with-file ()
  "Like `diff-buffer-with-file', but use current buffer without prompting."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer isn't associated with a file"))
  (diff-buffer-with-file (current-buffer)))

(defun km/diff--with-other-window (diff-func)
  (let ((windows (window-list)))
       (unless (= (length windows) 2)
         (user-error "Function restricted to two-window frames"))
       (-if-let* ((file-a (buffer-file-name
                             (window-buffer (car windows))))
                  (file-b (buffer-file-name
                           (window-buffer (cadr windows)))))
           (funcall diff-func file-a file-b)
         (user-error "At least one buffer is not visiting a file"))))

;;;###autoload
(defun km/diff-with-other-window ()
  "Run `diff' on current window's file and other window's file."
  (interactive)
  (km/diff--with-other-window #'diff))

;;;###autoload
(defun km/ediff-with-other-window ()
  "Run `ediff' on current window's file and other window's file."
  (interactive)
  (km/diff--with-other-window #'ediff))

(defvar km/ediff-previous-window-config nil)

;;;###autoload
(defun km/ediff-save-window-config ()
  (setq km/ediff-previous-window-config (current-window-configuration)))

;;;###autoload
(defun km/ediff-restore-window-config ()
  (when km/ediff-previous-window-config
    (set-window-configuration km/ediff-previous-window-config)
    (setq km/ediff-previous-window-config nil)))

;;; Diff review

(defvar km/diff-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-w") #'km/diff-review-copy-comment)
    (define-key map (kbd "C-c C-n") #'km/diff-review-next-comment)
    (define-key map (kbd "C-c C-p") #'km/diff-review-previous-comment)
    (define-key map (kbd "M-o") #'km/diff-review-open-line)
    map)
  "Keymap for Diff Review mode.")

(defvar km/diff-review-font-lock-keywords
  '(("^:.*$" . 'font-lock-function-name-face)))

;;;###autoload
(define-minor-mode km/diff-review-mode
  "Minor mode for reviewing diffs.

When this mode is turned on

  * `auto-fill-mode' is enabled

  * the buffer is editable

  * \":\" is set as the comment character

\\{km/diff-review-mode-map}"
  nil
  " DiffRev"
  km/diff-review-mode-map
  (if km/diff-review-mode
      (progn
        (auto-fill-mode)
        (set (make-local-variable 'comment-start) ": ")
        (setq fill-column (+ 2 fill-column))
        (font-lock-add-keywords nil km/diff-review-font-lock-keywords)
        (setq buffer-read-only nil)
        (if (not diff-update-on-the-fly)
            (remove-hook 'write-contents-functions 'diff-write-contents-hooks t)
          (remove-hook 'after-change-functions 'diff-after-change-function t)
          (remove-hook 'post-command-hook 'diff-post-command-hook t)))
    (font-lock-remove-keywords nil km/diff-review-font-lock-keywords)
    (diff-mode))
  (when font-lock-mode
    (font-lock-flush)))

(defun km/diff-review--comment-bounds ()
  (and (eq ?: (char-after (point-at-bol)))
       (cons
        (save-excursion (goto-char (point-at-bol))
                        (if (bobp)
                            (point)
                          (while (and (eq ?: (char-after))
                                      (not (bobp)))
                            (forward-line -1))
                          (forward-line)
                          (point)))
        (save-excursion (goto-char (point-at-bol))
                        (while (and (eq ?: (char-after))
                                    (not (eobp)))
                          (forward-line 1))
                        (forward-line -1)
                        (point-at-eol)))))

(defun km/diff-review-copy-comment ()
  "Copy the comment at point, stripping the leading ': '.
When there is no comment at point or when the region is active,
fall back to `kill-region'."
  (interactive)
  (let (bounds)
    (if (or (use-region-p)
            (not (setq bounds (km/diff-review--comment-bounds))))
        (call-interactively #'kill-region)
      (message
       "%s"
       (kill-new
        (thread-last
            (buffer-substring-no-properties (car bounds) (cdr bounds))
          (replace-regexp-in-string "^: ?" "")
          (replace-regexp-in-string "\\`\\s-+" "")
          (replace-regexp-in-string "\\s-+\\'" "")))))))

(defun km/diff-review-as-mail ()
  "Export the current diff review buffer as a mail reply."
  (interactive)
  (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
    (with-current-buffer (get-buffer-create "*Diff Review Mail*")
      (erase-buffer)
      (insert contents)
      (goto-char (point-min))
      (while (not (eobp))
        (insert "> ")
        (forward-line 1))
      (goto-char (point-min))
      (while (re-search-forward "^> : ?" nil t)
        (replace-match ""))
      (message-mode)
      (set-buffer-modified-p nil)
      (pop-to-buffer (current-buffer)))))

(defun km/diff-review-next-comment (&optional n)
  "Move to the Nth next comment.
If N is negative, move backward instead."
  (interactive "p")
  (when (/= n 0)
    (pcase-let ((`(,search-fn ,bound-fn ,incr-func)
                 (if (< n 0)
                     (list #'re-search-backward #'car #'1+)
                   (list #'re-search-forward #'cdr #'1-))))
      (letrec ((move
                (lambda (n)
                  (let ((bounds (km/diff-review--comment-bounds)))
                    (cond ((and (= n 0) bounds)
                           (goto-char (car bounds)))
                          ((/= n 0)
                           (when bounds
                             (goto-char (funcall bound-fn bounds)))
                           (and (funcall search-fn "^:" nil t)
                                (funcall move
                                         (funcall incr-func n))))
                          (t
                           (error "Error in movement logic")))))))
        (funcall move n)))))

(defun km/diff-review-previous-comment (&optional n)
  "Move to the Nth previous comment.
If N is negative, move to forward instead."
  (interactive "p")
  (km/diff-review-next-comment (- n)))

(defun km/diff-review-open-line ()
  "Insert a comment line above point.
If point is currently on a comment block, insert a single line.
Otherwise, insert a block of three comment lines and position
point at the middle line."
  (interactive)
  (goto-char (point-at-bol))
  (if (eq ?: (char-after))
      (progn (insert ": \n")
             (backward-char))
    (insert ": \n: \n: \n")
    (backward-char 4)))

(provide 'km-diff)
;;; km-diff.el ends here
