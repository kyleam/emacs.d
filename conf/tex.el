;; tex.el

;;; add path for pdflatex
(getenv "PATH")
(setenv "PATH"
        (concat
         "/usr/texbin" ":"
         (getenv "PATH")))

;; for viewing in-frame
;;http://www.sigmafield.org/2009/10/03/using-doc-view-with-auto-revert-to-view-latex-pdf-output-in-emacs/
                                        ;(setq TeX-PDF-mode t)
                                        ;(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; reftex
;; mostly from http://www.kieranhealy.org/esk/starter-kit-latex.html
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
;; (autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
;; (autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
;; (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
;; (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
;; (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; ;; Make RefTeX faster
;; (setq reftex-enable-partial-scans t)
;; (setq reftex-save-parse-info t)
;; (setq reftex-use-multiple-selection-buffers t)

;; commented out the rest of it because something was causing it to
;; prompt for master file in org mode
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  ;; (and (buffer-file-name)
  ;;      (file-exists-p (buffer-file-name))
  ;;      (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c [") 'reftex-citation)
  )

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(setq reftex-default-bibliography
      (quote
       ("~/refs/refs.bib")))
