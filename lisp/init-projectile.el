(require-package 'projectile)

(projectile-global-mode)

(setq projectile-switch-project-action 'projectile-commander
      projectile-find-dir-includes-top-level t
      projectile-use-git-grep t)

;; Default binding is D.
(def-projectile-commander-method ?r
  "Open project root in dired."
  (projectile-dired))

(def-projectile-commander-method ?c
  "Run project compilation command."
  (call-interactively 'projectile-compile-project))

(def-projectile-commander-method ?F
  "Find project file in other window."
  (call-interactively 'projectile-find-file-other-window))

(def-projectile-commander-method ?B
  "Find project buffer in other window."
  (call-interactively 'projectile-switch-to-buffer-other-window))

(def-projectile-commander-method ?D
  "Find a project directory in other window."
  (call-interactively 'projectile-find-dir-other-window))

(def-projectile-commander-method ?O
  "Display a project buffer in other window."
  (call-interactively 'projectile-display-buffer))

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

(define-key projectile-mode-map (kbd "C-c p j")
  'km/projectile-switch-project-to-file)

(key-chord-define-global ";s" 'projectile-switch-project)
(key-chord-define-global ";f" 'projectile-find-file)
(key-chord-define-global ";d" 'projectile-find-dir)
(key-chord-define-global ";t" 'km/projectile-open-external-terminal-in-root)
(key-chord-define-global ";g" 'projectile-grep)
(key-chord-define-global ";w" 'projectile-multi-occur)
(key-chord-define-global ";r" 'projectile-recentf)
(key-chord-define-global ";c" 'projectile-commander)

(define-prefix-command 'projectile-ctl-x-4-map)
(define-key ctl-x-4-map "p" 'projectile-ctl-x-4-map)

(define-key projectile-ctl-x-4-map (kbd "C-o")
  'projectile-display-buffer)
(define-key projectile-ctl-x-4-map "b"
  'projectile-switch-to-buffer-other-window)
(define-key projectile-ctl-x-4-map "d"
  'projectile-find-dir-other-window)
(define-key projectile-ctl-x-4-map "f"
  'projectile-find-file-other-window)
(define-key projectile-ctl-x-4-map "t"
  'projectile-find-implementation-or-test-other-window)

(provide 'init-projectile)
