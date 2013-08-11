;; http://www.emacswiki.org/emacs/PythonProgrammingInEmacs#toc5
(defun km/setup-ipython-shell ()
  (interactive)
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(defun km/python-hook ()
  (local-set-key (kbd "C-c m t") '(lambda ()
                                    (interactive)
                                    (compile "py.test")))
  (local-set-key (kbd "C-c m T") '(lambda ()
                                    (interactive)
                                    (compile "py.test2"))))

(add-hook 'python-mode-hook 'km/python-hook)
