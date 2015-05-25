
(autoload 'R-mode "ess-site")

(setq ess-smart-S-assign-key ";")

(setq ess-use-ido nil)

(after 'ess-mode
  (ess-add-style
   'km
   '((ess-first-continued-statement-offset . 4)
     (ess-continued-statement-offset . 0)
     (ess-arg-function-offset-new-line . nil)
     (ess-arg-function-offset . nil)
     (ess-expression-offset . nil)
     (ess-indent-level . 4)
     (ess-brace-offset . 0)
     (ess-else-offset . 0)
     (ess-close-brace-offset . 0))))
(setq ess-default-style 'km)

(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

(define-abbrev-table 'ess-mode-abbrev-table
  '(("true" "TRUE")
    ("false" "FALSE"))
  :system t)

(dolist (hook '(ess-mode-hook inferior-ess-mode-hook))
  (add-hook hook (lambda ()
                   (setq local-abbrev-table ess-mode-abbrev-table)))
  (add-hook hook 'abbrev-mode))

(defun km/ess-eval-buffer-up-to-line ()
  "Send up to the current line to inferior ESS process."
  (interactive)
  (ess-eval-region (point-min) (line-end-position) nil))

(defvar km/ess-dplry-pipe-key "|")

(defun km/ess-insert-dplyr-pipe ()
  "Insert `km/ess-dplry-pipe' using `ess-smart-S-assign'.
Based on instructions in `ess-smart-S-assign-key', I didn't think
this would work, but it seems to so far."
  (interactive)
  (let ((ess-S-assign " %>% ")
        (ess-smart-S-assign-key km/ess-dplry-pipe-key))
    (call-interactively #'ess-smart-S-assign)))

(after 'ess-mode
  (define-key ess-mode-map (kbd "C-c C-.") 'km/ess-eval-buffer-up-to-line)
  (define-key ess-mode-map "|" 'km/ess-insert-dplyr-pipe))

(after 'ess-inf
  (define-key inferior-ess-mode-map "|" 'km/ess-insert-dplyr-pipe))

(provide 'init-ess)
