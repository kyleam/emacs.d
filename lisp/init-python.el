
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'auto-complete-mode)

(setq jedi:tooltip-method nil
      ac-auto-start nil)

;; http://www.emacswiki.org/emacs/PythonProgrammingInEmacs#toc5
(defun km/setup-ipython-shell ()
  (interactive)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code
        "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

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

(defun km/python-shell-send-buffer-up-to-point ()
  "Send from beginning of buffer up until point to Python shell."
  (interactive)
  (save-excursion
    (setq end (point))
    (goto-char (point-min))
    (python-shell-send-region (point) end)))

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

(define-prefix-command 'km/python-prefix-map)
(define-key km/python-prefix-map "t" 'km/find-python-test-file-other-window)

(after 'python
  (define-key python-mode-map (kbd "C-c m") 'km/python-prefix-map)

  ;; Rebind `python-shell-send-buffer'.
  (define-key python-mode-map (kbd "C-c C-c")
    'km/python-shell-send-function-or-paragraph-and-step)
  (define-key python-mode-map (kbd "C-c C-b") 'python-shell-send-buffer)
  (define-key python-mode-map (kbd "C-c C-.")
    'km/python-shell-send-buffer-up-to-point)

  (key-chord-define python-mode-map ";w" 'auto-complete)

  ;; Swap `python-shell-send-defun' and `python-eldoc-at-point'.
  (define-key python-mode-map (kbd "C-c C-f") 'python-shell-send-defun)
  (define-key python-mode-map (kbd "C-M-x") 'python-eldoc-at-point))

(defun km/python-hook ()
  (set (make-local-variable 'compile-command) "py.test"))

(add-hook 'python-mode-hook 'km/python-hook)

(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

(setq python-fill-docstring-style 'pep-257-nn)

(provide 'init-python)
