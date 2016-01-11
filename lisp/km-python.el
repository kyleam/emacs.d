;;; km-python.el --- Python extensions

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

(require 'python)
(require 'km-util)

;;;###autoload
(defun km/toggle-ipython-shell ()
  "Switch between using python and IPython for interactive shell."
  (interactive)
  (setq python-shell-interpreter
        (if (string= python-shell-interpreter "python") "ipython" "python")))

;;;###autoload
(defun km/find-python-test-file-other-window (arg)
  "Open test file for the current Python file in another window.
If the file does not already exist, import the original Python
file. Unless ARG is non-nil, py.test is also imported."
  (interactive "P")
  (let* ((py-file (file-name-nondirectory buffer-file-name))
         (test-file (concat "test_" py-file)))
    (find-file-other-window test-file)
    (unless (file-exists-p test-file)
      (insert (format "import %s\n" (file-name-sans-extension py-file)))
      (unless arg
        (insert "import pytest\n")))))

;;;###autoload
(defun km/python-shell-send-function-or-paragraph-and-step ()
  "Send function or paragraph to Python shell.

Send function if point is inside one. Otherwise, send the current
paragraph. After evaluation, step to the next code line.

This is inspired by `ess-eval-function-or-paragraph-and-step'."
  (interactive)
  (if (km/python-inside-defun-p)
      (progn
        (python-shell-send-defun 1)
        (python-nav-end-of-defun))
    (let (end)
      (save-excursion
        (forward-paragraph)
        (setq end (point))
        (backward-paragraph)
        (python-shell-send-region (point) end))
      (goto-char end)))
  (km/python-next-code-line 1))

;;;###autoload
(defun km/python-shell-send-buffer-up-to-line ()
  "Send beginning of buffer to the current line to Python shell."
  (interactive)
  (python-shell-send-region (point-min) (line-end-position)))

(defun km/python-inside-defun-p ()
  ;; I don't use `python-nav-beginning-of-defun' because it will go to
  ;; the function even when the point is not inside of it.
  (when (python-info-current-defun)
    t))

(defun km/python-next-code-line (&optional arg skip-to-eob)
  "This is copied nearly exactly from `ess-next-code-line'."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((pos (point))
        (n 0)
        (inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc)); n=0 is success
      (if (not (fboundp 'comment-beginning))
          (while (and (= n 0)
                      (looking-at "\\s-*\\($\\|\\s<\\)"))
            (setq n (forward-line inc)))
        (comment-beginning)
        (beginning-of-line)
        (forward-comment (* inc (buffer-size))) ;; as suggested in info file
        )
      (if (or skip-to-eob
              (not (looking-at "[ \t\n]*\\'"))) ;; don't go to eob or whatever
          (setq arg (- arg inc))
        (goto-char pos)
        (setq arg 0)
        (forward-line 1));; stop at next empty line
      (setq pos (point)))
    (goto-char pos)
    n))

(defvar km/python-shell-current-string nil)

;;;###autoload
(defun km/python-shell-send-set-string (set)
  "Send previously set string to Python shell.
If a string has not been set previously or SET is non-nil, prompt
for a new string."
  (interactive "P")
  (when (or set (not km/python-shell-current-string))
      (let ((initial (and (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning) (region-end)))))
        (setq km/python-shell-current-string (read-string "Python command: " initial))))
  (python-shell-send-string km/python-shell-current-string))

(defun km/python-shell--read-buffer ()
  (let ((buf-alist (mapcar (lambda (b) (cons (buffer-name b) b))
                           (km/mode-buffers 'inferior-python-mode))))
    (if (= (length buf-alist) 1)
        (cdr (car buf-alist))
      (cdr (assoc-string (completing-read "Shell buffer: " buf-alist)
                         buf-alist)))))

(defun km/python-shell--find-prompt-start (move-func)
  "Find first line of input prompt.

`comint-forward-prompt' can end up at other points in input
entry, like '*':

    In [1]: for i in range(3):
       ...: *   print(i)

Return point at the beginning input entry."
  (let ((beg-prompt-re "\\(In \\[[0-9]+\\]:\\|>>> \\)"))
    (funcall move-func)
    (while (not (save-excursion
                  (beginning-of-line)
                  (looking-at-p beg-prompt-re)))
      (funcall move-func))
    (point-at-bol)))

;;;###autoload
(defun km/python-copy-last-shell-line-as-comment (&optional which-shell)
  "Insert last input and output Python shell as comment.
When the current buffer is not associated with a Python shell or
WHICH-SHELL is non-nil, prompt with all Python shell buffers."
  (interactive "P")
  (let* ((default-shell-buffer
           (get-buffer (format "*%s*" (python-shell-get-process-name t))))
         (shell-buffer (if (or which-shell (not default-shell-buffer))
                           (km/python-shell--read-buffer)
                         default-shell-buffer))
         (start-pos (point))
         text)
    (with-current-buffer shell-buffer
      (save-excursion
        (goto-char (point-max))
        (let* ((inhibit-field-text-motion t)
               (beg (km/python-shell--find-prompt-start
                     (lambda ()
                       (comint-previous-prompt 1))))
               (end (km/python-shell--find-prompt-start
                     ;; FIXME: If extra blank prompts exist at the end
                     ;; of shell, this will grab them (because
                     ;; `comint-next-prompt' skips over them).
                     (lambda ()
                       (end-of-line)
                       (comint-next-prompt 1)))))
          (setq text (buffer-substring-no-properties beg end)))))
    (insert text)
    (comment-region start-pos (point))))

(defun km/python-indent-post-self-insert-function ()
  "Adjust indentation after insert of specfic characters.
This is taken from `python-indent-post-self-insert-function'.
Unlike that function, it does not rely on `electric-indent-mode'
being turned on."
  (when (and (not (bolp))
             (let ((paren-start (python-syntax-context 'paren)))
               ;; Check that point is inside parens.
               (when paren-start
                 (not
                  ;; Filter the case where input is happening in the same
                  ;; line where the open paren is.
                  (= (line-number-at-pos)
                     (line-number-at-pos paren-start)))))
             ;; When content has been added before the closing paren or a
             ;; comma has been inserted, it's ok to do the trick.
             (or
              (memq (char-after) '(?\) ?\] ?\}))
              (eq (char-before) ?,)))
    (save-excursion
      (goto-char (line-beginning-position))
      (let ((indentation (python-indent-calculate-indentation)))
        (when (< (current-indentation) indentation)
          (indent-line-to indentation))))))


;;; Pydoc

(require 'pydoc)

(defvar km/pydoc-names nil
  "List of names that have been sucessfully loaded by `pydoc'.")

(defvar km/pydoc-names-file "~/.emacs.d/.pydoc-names"
  "File to save `km/pydoc-names' to.")

;;;###autoload
(defun km/pydoc ()
  "Run `pydoc', prompting with `km/pydoc-names'."
  (interactive)
  (let* ((default-directory "~/")
         (initial-name (and (use-region-p)
                            (buffer-substring-no-properties
                             (region-beginning)
                             (region-end))))
         (name (completing-read "Name: " km/pydoc-names nil nil
                                initial-name)))
    (pydoc name)))

(defun km/pydoc-store-name ()
  "Store the name for the current pydoc object."
  (with-current-buffer (pydoc-buffer)
    (unless (eq (plist-get pydoc-info :type) 'not-found)
      (cl-pushnew (substring-no-properties (car (cdr help-xref-stack-item)))
                  km/pydoc-names
                  :test #'string=))))

(defun km/pydoc-save-names-file (&optional file)
  "Save `km/pydoc-names' to FILE.
FILE is `km/pydoc-names-file' by default."
  (interactive
   (list
    (read-file-name (format "Save file (default %s): "
                            km/pydoc-names-file)
                    nil km/pydoc-names-file t)))
  (setq file (or file km/pydoc-names-file))
  (when (file-writable-p file)
    (with-temp-file file
      (let (print-length)
        (print (sort km/pydoc-names #'string-lessp)
               (current-buffer))))))

;;;###autoload
(defun km/pydoc-read-names-file (&optional file)
  "Read `km/pydoc-names-file' from FILE.
FILE is `km/pydoc-names-file' by default."
  (interactive
   (list
    (read-file-name (format "Read file (default %s): "
                            km/pydoc-names-file)
                    nil km/pydoc-names-file t)))
  (with-temp-buffer
    (insert-file-contents (or file km/pydoc-names-file))
    (setq km/pydoc-names (read (current-buffer)))))

(provide 'km-python)
;;; km-python.el ends here
