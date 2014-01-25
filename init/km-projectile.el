(projectile-global-mode)

(setq projectile-switch-project-action 'projectile-commander)

;; Default binding is D.
(def-projectile-commander-method ?r
  "Open project root in dired."
  (projectile-dired))

(def-projectile-commander-method ?c
  "Run project compilation command."
  (call-interactively 'projectile-compile-project))

(defun km/projectile-switch-project-to-file ()
  "Provide access to the of default `projectile-find-file'.

I have set `projectile-switch-project-action' to
`projectile-commander' but would still like quick access to
`projectile-find-file'."
  (interactive)
  (let ((projectile-switch-project-action 'projectile-find-file))
    (projectile-switch-project)))

(define-key projectile-mode-map (kbd "C-c p j")
  'km/projectile-switch-project-to-file)

(setq projectile-use-git-grep t)
