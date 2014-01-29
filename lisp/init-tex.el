(require-package 'auctex)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;; Add path for pdflatex.
(setenv "PATH"
        (concat
         "/usr/texbin" ":"
         (getenv "PATH")))

(defun km/org-mode-reftex-setup ()
  (define-key org-mode-map (kbd "C-c [") 'reftex-citation))

(add-hook 'org-mode-hook 'km/org-mode-reftex-setup)

(setq reftex-default-bibliography '("~/refs/refs.bib"))

(defun km/latex-narrow-to-single-tag (&optional tag)
  "Narrow region to LaTeX tag.
This is meant for single elements, like \"\\section\". If TAG is
not given, it is taken from the active region."
  (interactive)
  (save-excursion
    (let* ((tag (or tag
                (buffer-substring-no-properties (mark) (point))))
           (tag (if (s-starts-with? "\\" tag)
                    tag
                  (concat "\\" tag)))
           (beg (progn (end-of-line)
                       (search-backward tag)))
           (end (progn (forward-line 1)
                       (unless (search-forward tag nil t)
                         (search-forward "\\end{document}"))
                       (forward-line -1)
                       (point))))
      (narrow-to-region beg end))))

(defun km/latex-narrow-to-paired-tag (&optional tag)
  "Narrow region to LaTeX tag.
This is meant for paired elements, like \"\\begin{document}\". If
TAG is not given, it is taken from the active region."
  (interactive)
  (save-excursion
    (let* ((tag (or tag
                (buffer-substring-no-properties (mark) (point))))
           (beg (progn (end-of-line)
                       (search-backward (format "\\begin{%s}" tag))))
           (end (progn (forward-line 1)
                       (search-forward (format "\\end{%s}" tag)))))
      (narrow-to-region beg end))))

(defun km/latex-narrow-to-document ()
  "Narrow region to LaTeX document text.
The point should be beyond \"\\begin{document}\"."
  (interactive)
  (km/latex-narrow-to-paired-tag "document"))

(defun km/latex-narrow-to-section ()
  "Narrow region to current LaTeX section.
The point should be beyond \"\\section\"."
  (interactive)
  (km/latex-narrow-to-single-tag "section"))

(provide 'init-tex)
