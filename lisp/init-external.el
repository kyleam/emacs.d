
(setq x-select-enable-clipboard t ; Share clipboard with system.
      x-select-enable-primary t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setq ispell-program-name "aspell")

(define-prefix-command 'km/external-map)
(global-set-key (kbd "C-c x") 'km/external-map)

(define-key km/external-map "w" 'woman)
(define-key km/external-map "i" 'ispell-buffer)


;;; Shells

(setq shell-command-switch "-ic")

(defvar km/terminal "urxvt")

(defun km/open-external-terminal ()
  (interactive)
  (start-process "ext-term" nil km/terminal))
(define-key km/external-map "t" 'km/open-external-terminal)

(defun km/zsh-ansi-term (&optional directory)
  "Open an ansi-term buffer running ZSH in DIRECTORY.

If a terminal for DIRECTORY already exists, switch to that
buffer.  If the current buffer is a terminal for DIRECTORY,
create an additional terminal.

By default, DIRECTORY is `default-directory'.

With a numeric prefix argument 0, prompt the user with existing
ZSH terminal directories.

With a C-u prefix argument, set DIRECTORY to the user home
directory.

With any other non-nil value, prompt for a directory."
  (interactive (km/zsh-ansi-term--args))
  (let* ((dir (abbreviate-file-name directory))
         (name (concat "zsh: " dir))
         (full-name (concat "*" name "*"))
         (default-directory dir))
    (cond
     ((string= (km/zsh-ansi-term-directory) dir)
      (ansi-term "/bin/zsh" name))
     ((get-buffer full-name)
      (switch-to-buffer full-name))
     (t
      (ansi-term "/bin/zsh" name)))))

(defun km/zsh-ansi-term-other-window (&optional directory)
  (interactive (km/zsh-ansi-term--args))
  (pop-to-buffer (save-window-excursion (km/zsh-ansi-term directory))))

(defun km/zsh-ansi-term--args ()
  (list (cond
         ((not current-prefix-arg)
          default-directory)
         ((= (prefix-numeric-value current-prefix-arg) 4)
          "~/")
         ((= (prefix-numeric-value current-prefix-arg) 0)
          (--if-let (km/zsh-ansi-term-current-directories)
              (completing-read "Directory: " it nil nil nil nil (car it))
            (user-error "No ZSH buffers found")))
         (t
          (read-directory-name "Directory: ")))))

(defun km/zsh-ansi-term-directory (&optional buffer)
  "Return directory name for ZSH terminal in BUFFER.
BUFFER defaults to current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((bname (buffer-name)))
      (and (derived-mode-p 'term-mode)
           (string-match "^\\*zsh: \\(.*\\)\\*\\(<[0-9]+>\\)*$"
                         bname)
           (match-string 1 bname)))))

(defun km/zsh-ansi-term-current-directories ()
  (-distinct (-keep #'km/zsh-ansi-term-directory (buffer-list))))

(define-key km/external-map "a" 'km/zsh-ansi-term)
(define-key km/external-map "r" 'shell-command-on-region)
(define-key km/external-map "s" 'shell-command)
(define-key km/external-map "S" 'shell)

;; This overrides binding for `add-change-log-entry-other-window'.
(define-key ctl-x-4-map "a" 'km/zsh-ansi-term-other-window)


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
(define-key km/compile-map "o" 'km/display-compilation-other-window)
(define-key km/compile-map "r" 'km/recompile-current-compilation)

(key-chord-define-global ",e" 'km/recompile-current-compilation)


;;; Diff

(setq diff-command "/bin/diff"
      diff-switches "-u")

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun km/diff ()
  "Run `diff` and then select buffer and turn on View mode."
  (interactive)
  (call-interactively #'diff)
  (select-window (get-buffer-window "*Diff*"))
  (view-mode 1))

(defun km/revert-buffer-and-view ()
  (interactive)
  (revert-buffer)
  (view-mode 1))
(after 'diff
  (define-key diff-mode-map (kbd "C-c C-g") 'km/revert-buffer-and-view))

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

(define-key km/external-map "d" 'km/diff)
(define-key km/external-map "e" 'ediff)
(define-key km/external-map "o" 'km/ediff-with-other-window)


;;; WebJump

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

(define-key km/external-map  "j" 'webjump)

(provide 'init-external)
