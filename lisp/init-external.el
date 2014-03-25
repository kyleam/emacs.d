(defvar km/terminal "urxvt")

(defun km/open-external-terminal ()
  (interactive)
  (start-process "ext-term" nil km/terminal))

(defadvice recompile (around restore-windows activate)
  "Prevent recompiling from spawning new windows."
  (save-window-excursion
    ad-do-it))

(defadvice shell-command (around shell-command-restore-windows activate)
  "Restore window configuraiton after shell-command.
The hides the *Async Shell Command* buffer that is opened in the
other window when an asynchronous command is run."
  (window-configuration-to-register :before-shell-command)
  ad-do-it
  (jump-to-register :before-shell-command))

(define-prefix-command 'external-map)
(global-set-key (kbd "C-c x") 'external-map)

(defun km/zsh-ansi-term ()
  (interactive)
  (ansi-term "/bin/zsh"))

(define-key external-map "a" 'km/zsh-ansi-term)
(define-key external-map "t" 'km/open-external-terminal)
(define-key external-map "r" 'shell-command-on-region)
(define-key external-map "s" 'shell-command)
(define-key external-map "S" 'shell)

(define-key external-map "c" 'compile)
(define-key external-map "g" 'recompile)

(define-key external-map "w" 'woman)

;; Give frequently-used recompile a shorter binding.
(global-set-key (kbd "C-c g") 'recompile)

(provide 'init-external)
