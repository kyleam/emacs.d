(defun km/python-sysexit ()
  (interactive)
  (insert "sys.exit()"))

(defun km/python-random-assignment ()
  (interactive)
  (km/insert-random-string 10)
  (insert " = None"))

(defun km/python-shebang ()
  (interactive)
  (km/shebang "python"))

(defun km/python-hook ()
  (local-set-key (kbd "C-c p r") 'km/python-random-assignment)
  (local-set-key (kbd "C-c p e") 'km/python-sysexit)
  (local-set-key (kbd "C-c p s") 'km/python-shebang))
(add-hook 'python-mode-hook 'km/python-hook)
