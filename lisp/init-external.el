(define-prefix-command 'external-map)
(global-set-key (kbd "C-c x") 'external-map)

;;; Terminals

(defvar km/terminal "urxvt")

(defun km/open-external-terminal ()
  (interactive)
  (start-process "ext-term" nil km/terminal))

(defadvice shell-command (around shell-command-restore-windows activate)
  "Restore window configuraiton after shell-command.
The hides the *Async Shell Command* buffer that is opened in the
other window when an asynchronous command is run."
  (window-configuration-to-register :before-shell-command)
  ad-do-it
  (jump-to-register :before-shell-command))

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

(defun km/zsh-ansi-term-other-window (&optional new-buffer)
  (interactive "P")
  (let (buffer-name)
    (save-window-excursion
      (setq buffer-name (km/zsh-ansi-term new-buffer)))
    (pop-to-buffer buffer-name)))

(define-key external-map "a" 'km/zsh-ansi-term)
;; This overrides binding for `add-change-log-entry-other-window'.
(define-key ctl-x-4-map "a" 'km/zsh-ansi-term-other-window)
(define-key external-map "t" 'km/open-external-terminal)
(define-key external-map "r" 'shell-command-on-region)
(define-key external-map "s" 'shell-command)
(define-key external-map "S" 'shell)

(define-key external-map "w" 'woman)

;;; Compilation

(defadvice recompile (around restore-windows activate)
  "Prevent recompiling from spawning new windows."
  (save-window-excursion
    ad-do-it))

(defun km/display-compilation-other-window ()
  (interactive)
  (-if-let (comp-buffer (get-buffer "*compilation*"))
      (display-buffer comp-buffer)
    (error "No compilation buffer")))

(define-key external-map "c" 'compile)
(define-key external-map "g" 'recompile)
(define-key external-map "o" 'km/display-compilation-other-window)
;; Give frequently-used recompile a shorter binding.
(global-set-key (kbd "C-c g") 'recompile)

;;; Diff

(setq diff-command "/bin/diff"
      diff-switches "-u")

(defadvice diff (after diff-select-and-view activate)
  (select-window (get-buffer-window "*Diff*"))
  (view-mode 1))

(defun km/revert-buffer-and-view ()
  (interactive)
  (revert-buffer)
  (view-mode 1))

(define-key external-map "d" 'diff)
(define-key external-map "e" 'ediff)
(define-key diff-mode-map (kbd "C-c C-g") 'km/revert-buffer-and-view)

(provide 'init-external)
