;;; add path for pdflatex
(getenv "PATH")
(setenv "PATH"
        (concat
         "/usr/texbin" ":"
         (getenv "PATH")))

;; commented out the rest of it because something was causing it to
;; prompt for master file in org mode
(defun km/org-mode-reftex-setup ()
  (load-library "reftex")
  ;; (and (buffer-file-name)
  ;;      (file-exists-p (buffer-file-name))
  ;;      (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c [") 'reftex-citation))

(add-hook 'org-mode-hook 'km/org-mode-reftex-setup)

(setq reftex-default-bibliography
      (quote
       ("~/refs/refs.bib")))
