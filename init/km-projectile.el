(projectile-global-mode)

(setq projectile-switch-project-action 'km/projectile-magit-status)

(defun km/projectile-magit-status ()
  (magit-status (projectile-project-root)))

(defun km/projectile-switch-project-to-file ()
  "I have set `projectile-switch-project-action' to
  `km/projectile-magit-status'. This function gives access to the
  of default `projectile-find-file'"
  (interactive)
  (let ((projectile-switch-project-action 'projectile-find-file))
    (projectile-switch-project)))

(global-set-key (kbd "C-c p j")  'km/projectile-switch-project-to-file)
