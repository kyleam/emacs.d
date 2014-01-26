(define-prefix-command 'external-map)
(global-set-key (kbd "C-c x") 'external-map)

(defvar km/terminal "urxvt")

(defun km/open-external-terminal ()
  (interactive)
  (start-process "ext-term" nil km/terminal))

(define-key external-map "t" 'km/open-external-terminal)
(define-key external-map "s" 'shell-command)

(defadvice recompile (around restore-windows)
  "Prevent recompiling from spawning new windows."
  (save-window-excursion
    ad-do-it))
(ad-activate 'recompile)

(global-set-key (kbd "C-c g") 'recompile)

(provide 'init-external)
