
(projectile-global-mode)

(setq projectile-switch-project-action 'projectile-commander
      projectile-find-dir-includes-top-level t
      projectile-use-git-grep t)

(defun km/projectile-switch-project-to-file ()
  "Provide access to the of default `projectile-find-file'.

I have set `projectile-switch-project-action' to
`projectile-commander' but would still like quick access to
`projectile-find-file'."
  (interactive)
  (let ((projectile-switch-project-action 'projectile-find-file))
    (projectile-switch-project)))

(defun km/projectile-open-external-terminal-in-root ()
  "Run `km/open-external-terminal' in project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (km/open-external-terminal)))

(defun km/projectile-view-file ()
  "View project file.
Interactive arguments are processed according to
`projectile-find-file'."
  (interactive)
  (call-interactively 'projectile-find-file)
  (view-mode 1))

(defun km/projectile-view-file-other-window ()
  "View project file in other window.
Interactive arguments are processed according to
`projectile-find-file-other-window'."
  (interactive)
  (call-interactively 'projectile-find-file-other-window)
  (view-mode 1))

(defun km/project-filename-at-point ()
  "Return file name relative to `projectile-project-root'."
  (--when-let (thing-at-point 'filename)
    (file-relative-name it (projectile-project-root))))

(defun km/projectile-copy-project-filename-as-kill ()
  (interactive)
  (let ((fname (km/project-filename-at-point)))
    (if (eq last-command 'kill-region)
        (kill-append fname nil)
      (kill-new fname))
    (message "%s" fname)))

;; Default binding is D.
(def-projectile-commander-method ?t
  "Open project root in dired."
  (projectile-dired))

(def-projectile-commander-method ?D
  "Find a project directory in other window."
  (call-interactively 'projectile-find-dir-other-window))

;; Default binding is v.
(def-projectile-commander-method ?m
  "Open project root in vc-dir or magit."
  (projectile-vc))

(def-projectile-commander-method ?v
  "View project file."
  (km/projectile-view-file))

(def-projectile-commander-method ?V
  "View project file in other window."
  (km/projectile-view-file-other-window))

(def-projectile-commander-method ?c
  "Run project compilation command."
  (call-interactively 'projectile-compile-project))

;; Default binding is e.
(def-projectile-commander-method ?r
  "Find recently visited file in project."
  (projectile-recentf))

(def-projectile-commander-method ?F
  "Find project file in other window."
  (call-interactively 'projectile-find-file-other-window))

(def-projectile-commander-method ?B
  "Find project buffer in other window."
  (call-interactively 'projectile-switch-to-buffer-other-window))

(def-projectile-commander-method ?O
  "Display a project buffer in other window."
  (call-interactively 'projectile-display-buffer))

(def-projectile-commander-method ?i
  "Open an IBuffer window showing all buffers in the current project."
  (call-interactively 'projectile-ibuffer))

(key-chord-define-global ";s" 'projectile-switch-project)
(key-chord-define-global ";f" 'projectile-find-file)
(key-chord-define-global ";v" 'km/projectile-view-file)
(key-chord-define-global ";d" 'projectile-find-dir)
(key-chord-define-global ";t" 'km/projectile-open-external-terminal-in-root)
(key-chord-define-global ";g" 'projectile-grep)
(key-chord-define-global ";r" 'projectile-recentf)
(key-chord-define-global ";c" 'projectile-commander)

(define-key projectile-command-map "j"
  'km/projectile-switch-project-to-file)
(define-key projectile-command-map "."
  'km/projectile-copy-project-filename-as-kill)

;; Swap `projectile-invalidate-cache' and `projectile-ibuffer'.
(define-key projectile-command-map "I" 'projectile-invalidate-cache)
(define-key projectile-command-map "i" 'projectile-ibuffer)

(define-key projectile-command-map (kbd "4 v")
  'km/projectile-view-file-other-window)

(define-prefix-command 'km/projectile-ctl-x-4-map)
(define-key ctl-x-4-map "p" 'km/projectile-ctl-x-4-map)

(define-key km/projectile-ctl-x-4-map (kbd "C-o")
  'projectile-display-buffer)
(define-key km/projectile-ctl-x-4-map "b"
  'projectile-switch-to-buffer-other-window)
(define-key km/projectile-ctl-x-4-map "d"
  'projectile-find-dir-other-window)
(define-key km/projectile-ctl-x-4-map "f"
  'projectile-find-file-other-window)
(define-key km/projectile-ctl-x-4-map "v"
  'km/projectile-view-file-other-window)
(define-key km/projectile-ctl-x-4-map "t"
  'projectile-find-implementation-or-test-other-window)

(provide 'init-projectile)
