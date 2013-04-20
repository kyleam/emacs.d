(defun km/python-sysexit ()
  (interactive)
  (insert "sys.exit()"))

(defun km/python-shebang ()
  (interactive)
  (km/shebang "python"))

(defconst km/python-analysis-imports
  "import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator

from colors import brew, fgry, bgry")

(defun km/python-insert-analysis-imports ()
  "Common imports for analysis scripts"
  (interactive)
  (insert km/python-analysis-imports))

(defun km/python-hook ()
  (local-set-key (kbd "C-c p e") 'km/python-sysexit)
  (local-set-key (kbd "C-c p s") 'km/python-shebang)
  (local-set-key (kbd "C-c p a") 'km/python-insert-analysis-imports))
(add-hook 'python-mode-hook 'km/python-hook)
