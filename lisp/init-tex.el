
(setq reftex-default-bibliography '("refs.bib"))

(setq font-latex-fontify-sectioning 'color
      TeX-electric-math '("$" . "$"))

(put 'LaTeX-narrow-to-environment 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.[tT]e[xX]\\'" . latex-mode))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq imenu-create-index-function
                                   #'km/latex-imenu-create-index-function)))

(defun km/latex-imenu-create-index-function ()
  ;; See `LaTeX-imenu-create-index-function'.
  (TeX-update-style)
  (let ((sec-re (LaTeX-outline-regexp))
        (title-re "\\*\\{0,1\\}{\\([^}]+\\)}")
        entries)
    (goto-char (point-min))
    (while (re-search-forward sec-re nil t)
      (let ((sec (replace-regexp-in-string
                  "\\\\" "" (match-string-no-properties 0)))
            (title (and (looking-at title-re)
                        (replace-regexp-in-string
                         "\\s-\\s-+" " "
                         (replace-regexp-in-string
                          "\n" "" (match-string-no-properties 1))))))
        (when (> (length title) 45)
          (setf (substring title 21 -21) "..."))
        (push (cons (if title (format "%s (%s)" title sec) sec)
                    (save-excursion (beginning-of-line) (point-marker)))
              entries)))
    (nreverse entries)))

(provide 'init-tex)
