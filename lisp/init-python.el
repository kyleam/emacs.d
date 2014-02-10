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

(defun km/create-python-test-file (arg)
  "Create a python test file from the name of the current file.
Unless a prefix argument ARG is given, py.test is also imported."
  (interactive "P")
  (let* ((py-file (file-name-nondirectory buffer-file-name))
         (test-file (concat "test_" py-file)))
    (find-file-other-window test-file)
    (unless (file-exists-p test-file)
      (with-current-buffer
          (insert (format "import %s\n" (file-name-sans-extension py-file)))
        (unless arg
          (insert "import pytest\n"))))))

(defun km/pytest-compile (arg)
  "Run pytest.
If ARG, use pytest2 instead of pytest."
  (interactive "P")
  (let ((pytest (if arg "py.test2" "py.test")))
    (compile pytest)))

(defun km/python-hook ()
  (local-set-key (kbd "C-c m c") 'km/create-python-test-file)
  (local-set-key (kbd "C-c m t") 'km/pytest-compile))

(add-hook 'python-mode-hook 'km/python-hook)

(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

(setq python-fill-docstring-style 'pep-257-nn)

(provide 'init-python)
