(define-prefix-command 'km/external-map)
(global-set-key (kbd "C-c x") 'km/external-map)

(setq shell-command-switch "-ic"
      x-select-enable-clipboard t ; Share clipboard with system.
      x-select-enable-primary t
      ispell-program-name "aspell"
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;;; Terminals

(defvar km/terminal "urxvt")

(defun km/open-external-terminal ()
  (interactive)
  (start-process "ext-term" nil km/terminal))

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

(defun km/ediff-with-other-window ()
  "Run Ediff on current window's file and other window's file."
  (interactive)
  (let ((windows (window-list)))
       (unless (= (length windows) 2)
         (user-error "Function restricted to two-window frames"))
       (-if-let* ((file-a (buffer-file-name
                             (window-buffer (car windows))))
                  (file-b (buffer-file-name
                           (window-buffer (cadr windows)))))
           (ediff file-a file-b)
         (user-error "At least one buffer is not visiting a file"))))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(define-key km/external-map "d" 'diff)
(define-key km/external-map "e" 'ediff)
(define-key km/external-map "o" 'km/ediff-with-other-window)
(after 'diff
  (define-key diff-mode-map (kbd "C-c C-g") 'km/revert-buffer-and-view))

;;; WebJump

(define-key km/external-map  "j" 'webjump)

(setq webjump-sites
  '(("Arch User Repository" .
     [simple-query "https://aur.archlinux.org"
                   "https://aur.archlinux.org/packages/?K=" ""])
    ("DuckDuckGo" .
     [simple-query "https://duckduckgo.com"
                   "https://duckduckgo.com/?q=" ""])
    ("Emacs Wiki" .
     [simple-query "www.emacswiki.org"
                   "www.emacswiki.org/cgi-bin/wiki/" ""])
    ("GitHub" . "https://github.com")
    ("GitHub search" .
     [simple-query "https://github.com"
                   "https://github.com/search?q=" ""])
    ("Google" .
     [simple-query "www.google.com"
                   "www.google.com/search?q=" ""])
    ("Google Scholar" .
     [simple-query "http://scholar.google.com"
                   "http://scholar.google.com/scholar?&q=" ""])
    ("Wikipedia" .
     [simple-query "wikipedia.org"
                   "wikipedia.org/wiki/" ""])))

;;; Misc

(define-key km/external-map "w" 'woman)
(define-key km/external-map "i" 'ispell-buffer)

(provide 'init-external)
