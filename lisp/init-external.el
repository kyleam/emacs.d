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

(defun km/zsh-ansi-term (&optional new-buffer)
  "Open an ansi-term buffer running ZSH.
The buffer is named according to `default-directory'. If a buffer
of the same name already exists, switch to it unless NEW-BUFFER
is non-nil, switch to the buffer."
  (interactive "P")
  (let ((name (concat "zsh:" (abbreviate-file-name default-directory))))
    (-if-let (buffer-name (and (not new-buffer)
                               (get-buffer (concat "*" name "*"))))
        (switch-to-buffer buffer-name)
      (ansi-term "/bin/zsh" name))))

(defun km/display-compilation-other-window ()
  (interactive)
  (-if-let (comp-buffer (get-buffer "*compilation*"))
      (display-buffer comp-buffer)
    (error "No compilation buffer")))

(define-key external-map "a" 'km/zsh-ansi-term)
(define-key external-map "t" 'km/open-external-terminal)
(define-key external-map "r" 'shell-command-on-region)
(define-key external-map "s" 'shell-command)
(define-key external-map "S" 'shell)

(define-key external-map "c" 'compile)
(define-key external-map "g" 'recompile)
(define-key external-map "o" 'km/display-compilation-other-window)

(define-key external-map "w" 'woman)

;; Give frequently-used recompile a shorter binding.
(global-set-key (kbd "C-c g") 'recompile)

(provide 'init-external)
