(defun km/python-shebang ()
  (interactive)
  (km/shebang "python"))

(defconst km/python-analysis-imports
  "import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import ticker

from colors import brew, fgry, bgry")

(defun km/python-insert-analysis-imports ()
  "Common imports for analysis scripts"
  (interactive)
  (insert km/python-analysis-imports))

(defun km/python-hook ()
  (local-set-key (kbd "C-c p s") 'km/python-shebang)
  (local-set-key (kbd "C-c p a") 'km/python-insert-analysis-imports))
(add-hook 'python-mode-hook 'km/python-hook)

;; http://www.emacswiki.org/emacs/PythonProgrammingInEmacs#toc5
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
  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
