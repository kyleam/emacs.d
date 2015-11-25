
(setq x-select-enable-clipboard t ; Share clipboard with system.
      x-select-enable-primary t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setq ispell-program-name "aspell")

(setq Man-notify-method 'aggressive)

(define-prefix-command 'km/external-map)
(global-set-key (kbd "C-c z") 'km/external-map)

(define-key km/external-map "b" 'browse-url)
(define-key km/external-map "m" 'helm-man-woman)
(define-key km/external-map "i" 'ispell-buffer)


;;; Shells

(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-c")

(defvar km/terminal "urxvt")

(defun km/open-external-terminal ()
  (interactive)
  (start-process "ext-term" nil km/terminal))
(define-key km/external-map "t" 'km/open-external-terminal)

(defun km/zsh-ansi-term (&optional directory name)
  "Open an ansi-term buffer running ZSH in DIRECTORY.

If a terminal for DIRECTORY already exists, switch to that
buffer.  If the current buffer is a terminal for DIRECTORY,
create an additional terminal.

By default, DIRECTORY is `default-directory'.

With a numeric prefix argument 0, prompt the user with existing
ZSH terminal directories.

With a C-u prefix argument, set DIRECTORY to the user home
directory.

With any other non-nil value, prompt for a directory.

If NAME is non-nil, use *NAME* for the buffer name instead of
*zsh: DIRECTORY*.  If that buffer already exists, it will be
grabbed regardless of whether its default-directory matches
DIRECTORY."
  (interactive (km/zsh-ansi-term--args))
  (let* ((dir (abbreviate-file-name directory))
         (name (or name (concat "zsh: " dir)))
         (full-name (concat "*" name "*"))
         (default-directory dir))
    (cond
     ((string= (km/zsh-ansi-term-directory) dir)
      (ansi-term "zsh" name))
     ((get-buffer full-name)
      (switch-to-buffer full-name))
     (t
      (ansi-term "zsh" name)))))

(defun km/zsh-toggle-ansi-term-home ()
  (interactive)
  (if (string= "*zsh*" (buffer-name))
      (bury-buffer)
    (km/zsh-ansi-term "~/" "zsh")))

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
          (let ((dirs (km/zsh-ansi-term-current-directories)))
            (cl-case (length dirs)
              (0 (user-error "No ZSH buffers found"))
              (1 (car dirs))
              (t (completing-read "Directory: " dirs
                                  nil nil nil nil (car dirs))))))
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

(key-chord-define-global "kz" 'km/zsh-toggle-ansi-term-home)

;; This overrides binding for `add-change-log-entry-other-window'.
(define-key ctl-x-4-map "a" 'km/zsh-ansi-term-other-window)


;;; Compilation

(defvar km/compilation-buffer-name-prefix "compilation: ")

(defun km/compilation-name-by-directory (&optional mode)
  (let ((name (if (and mode (not (equal mode "compilation")))
                  (downcase mode)
                (concat km/compilation-buffer-name-prefix
                        (abbreviate-file-name default-directory)))))
    (concat "*" name "*")))

(setq compilation-buffer-name-function 'km/compilation-name-by-directory)

(defun km/compilation-buffer-p (buffer)
  (with-current-buffer buffer
    (and (derived-mode-p 'compilation-mode)
         (string-prefix-p (concat "*" km/compilation-buffer-name-prefix)
                          (buffer-name)))))

(defadvice compile (around prevent-duplicate-compilation-windows activate)
  "Pop to compilation buffer only if it isn't visible.
This is useful for using multiple frames (e.g., with a two
monitor setup)."
  (if (get-buffer-window (km/compilation-name-by-directory)
                         'visible)
      (save-window-excursion ad-do-it)
    ad-do-it))

(defun km/compile-in-home-dir ()
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively #'compile)))

(defadvice recompile (around prevent-window-on-compilation activate)
  "Prevent recompiling from spawning new windows."
  (save-window-excursion ad-do-it))

(defun km/compilation-recompile (&optional arg)
  "Recompile buffer.
By default, use `compilation-last-buffer'.  If ARG is 0, get
buffer with name given by `km/compilation-name-by-directory'.
Otherwise, if ARG is non-nil, prompt with buffers from
`km/compilation-buffer-list'."
  (interactive (list (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (with-current-buffer (km/compilation--get-buffer arg)
    (if (derived-mode-p 'occur-mode)
        (revert-buffer)
      (recompile))))

(defun km/compilation-display-buffer (&optional arg)
  "Display compilation buffer.
By default, use `compilation-last-buffer'.  If ARG is 0, get
buffer with name given by `km/compilation-name-by-directory'.
Otherwise, if ARG is non-nil, prompt with buffers from
`km/compilation-buffer-list'."
  (interactive (list (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (display-buffer (km/compilation--get-buffer arg)))

(defun km/compilation--get-buffer (&optional arg)
  (cond
   ((and (not arg)
         (buffer-live-p compilation-last-buffer)
         compilation-last-buffer))
   ((and (numberp arg)
         (= arg 0))
    (get-buffer (km/compilation-name-by-directory)))
   (t
    (let ((cbufs (-map #'buffer-name (km/compilation-buffer-list)))
          buf)
      (cl-case (length cbufs)
        (0 (user-error "No compilation buffers found"))
        (1 (setq buf (car cbufs)))
        (t (setq buf (completing-read "Compilation buffer: " cbufs
                                      nil nil nil nil (car cbufs)))))
      buf))))

(defun km/compilation-buffer-list ()
  (-filter #'km/compilation-buffer-p (buffer-list)))

(define-prefix-command 'km/compile-map)
(global-set-key (kbd "C-c c") 'km/compile-map)

(define-key km/compile-map "c" 'compile)
(define-key km/compile-map "g" 'recompile)
(define-key km/compile-map "h" 'km/compile-in-home-dir)
(define-key km/compile-map "o" 'km/compilation-display-buffer)

(key-chord-define-global "hv" 'km/compilation-recompile)


;;; Diff

(setq diff-command "/bin/diff"
      diff-switches "-u")

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun km/diff ()
  "Run `diff' and then select buffer and turn on View mode."
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
  "Run `ediff' on current window's file and other window's file."
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

(defun km/webjump-read-string (prompt)
  "Like `webjump-read-string', but set default."
  (let* ((default (if (use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (thing-at-point 'symbol)))
         (prompt (if default
                     (format "%s (%s): " prompt default)
                   (concat prompt ": ")))
         (input (read-string prompt nil nil default)))
    (unless (webjump-null-or-blank-string-p input)
      (substring-no-properties input))))

(defun km/webjump ()
  "Run`webjump' with symbol at point or region as default query.
This affects only sites in the `simple-query' format."
  (interactive)
  (cl-letf (((symbol-function 'webjump-read-string) #'km/webjump-read-string))
    (call-interactively #'webjump)))

(define-key km/external-map  "j" 'km/webjump)

(provide 'init-external)
