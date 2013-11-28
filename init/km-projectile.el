(projectile-global-mode)

(setq projectile-switch-project-action 'km/projectile-magit-status)

(defun km/projectile-magit-status ()
  (magit-status (projectile-project-root)))
