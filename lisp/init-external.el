(define-prefix-command 'km/external-map)
(global-set-key (kbd "C-c x") 'km/external-map)

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
is non-nil."
  (interactive "P")
  (let ((name (concat "zsh:" (abbreviate-file-name default-directory))))
    (--if-let (and (not new-buffer)
                   (get-buffer (concat "*" name "*")))
        (switch-to-buffer it)
      (ansi-term "/bin/zsh" name))))

(defun km/zsh-ansi-term-other-window (&optional new-buffer)
  (interactive "P")
  (let (buffer-name)
    (save-window-excursion
      (setq buffer-name (km/zsh-ansi-term new-buffer)))
    (pop-to-buffer buffer-name)))

(define-key km/external-map "a" 'km/zsh-ansi-term)
;; This overrides binding for `add-change-log-entry-other-window'.
(define-key ctl-x-4-map "a" 'km/zsh-ansi-term-other-window)
(define-key km/external-map "t" 'km/open-external-terminal)
(define-key km/external-map "r" 'shell-command-on-region)
(define-key km/external-map "s" 'shell-command)
(define-key km/external-map "S" 'shell)

(define-key km/external-map "w" 'woman)

(define-key km/external-map "i" 'ispell-buffer)

;;; Compilation

(defadvice compile (around prevent-duplicate-compilation-windows activate)
  "Pop to compilation buffer only if it isn't visible.
This is useful for using multiple frames (e.g., with a two
monitor setup)."
  (if (get-buffer-window "*compilation*" 'visible)
      (save-window-excursion
        ad-do-it)
    ad-do-it))

(defadvice recompile (around prevent-window-on-compilation activate)
  "Prevent recompiling from spawning new windows."
  (save-window-excursion
    ad-do-it))

(defun km/display-compilation-other-window ()
  (interactive)
  (--if-let (get-buffer "*compilation*")
      (display-buffer it)
    (user-error "No compilation buffer")))

(defun km/recompile-current-compilation ()
  (interactive)
  (--if-let (get-buffer "*compilation*")
      (with-current-buffer it
        (recompile))
    (user-error "No compilation buffer")))

(define-prefix-command 'km/compile-map)
(global-set-key (kbd "C-c c") 'km/compile-map)

(define-key km/compile-map "c" 'compile)
(define-key km/compile-map "g" 'recompile)
(define-key km/compile-map "r" 'km/recompile-current-compilation)
(define-key km/compile-map "o" 'km/display-compilation-other-window)

(key-chord-define-global ",e" 'km/recompile-current-compilation)

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

(define-key km/external-map "d" 'diff)
(define-key km/external-map "e" 'ediff)
(after 'diff
  (define-key diff-mode-map (kbd "C-c C-g") 'km/revert-buffer-and-view))

(provide 'init-external)
