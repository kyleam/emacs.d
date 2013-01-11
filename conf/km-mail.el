(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

(defun km/mail-position ()
  "Move cursor to first blank line
and position cursor between two blank lines"
  (interactive)
  (forward-paragraph)
  (insert "\n\n")
  (previous-line))

(defun km/mail-mode-hook ()
  (auto-fill-mode 1)
  (km/mail-position))

(add-hook 'mail-mode-hook 'km/mail-mode-hook)
