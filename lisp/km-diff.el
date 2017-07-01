;;; km-diff.el --- Diff-related extensions

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

  * typing \\<km/diff-review-mode-map>\\[km/diff-review-copy-comment] \
will copy and the current comment,
     minus the comment character"
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

(defun km/diff-review-copy-comment ()
  (interactive)
  (if (or (use-region-p)
          (not (eq ?: (char-after (point-at-bol)))))
      (call-interactively #'kill-region)
    (let ((beg (save-excursion (goto-char (point-at-bol))
                               (if (bobp)
                                   (point)
                                 (while (and (eq ?: (char-after))
                                             (not (bobp)))
                                   (forward-line -1))
                                 (forward-line)
                                 (point))))
          (end (save-excursion (goto-char (point-at-bol))
                               (while (and (eq ?: (char-after))
                                           (not (eobp)))
                                 (forward-line 1))
                               (forward-line -1)
                               (point-at-eol))))
      (message
       "%s"
       (kill-new
        (thread-last
            (buffer-substring-no-properties beg end)
          (replace-regexp-in-string "^: ?" "")
          (replace-regexp-in-string "\\`\\s-+" "")
          (replace-regexp-in-string "\\s-+\\'" "")))))))

(defun km/diff-review-as-mail ()
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

(provide 'km-diff)
;;; km-diff.el ends here
