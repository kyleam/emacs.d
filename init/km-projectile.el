(projectile-global-mode)

(setq projectile-switch-project-action 'projectile-commander)

;; default binding is D
(def-projectile-commander-method ?r
  "Open project root in dired."
  (projectile-dired))

(defun km/projectile-switch-project-to-file ()
  "Provide access to the of default `projectile-find-file'.

I have set `projectile-switch-project-action' to
`projectile-commander' but would still like quick access to
`projectile-find-file'"
  (interactive)
  (let ((projectile-switch-project-action 'projectile-find-file))
    (projectile-switch-project)))

(define-key projectile-mode-map (kbd "C-c p j")
  'km/projectile-switch-project-to-file)

(setq projectile-use-git-grep t)

(defun km/projectile-query-and-replace (from to &optional delimited)
  "Peform query and replace on current project files.

Core logic taken from `dired-do-query-replace-regexp', replacing
the marked dired file list with the list from
`projectile-current-project-files'."
  (interactive
   (let ((common
          (query-replace-read-args
           "Query replace regexp in project files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (projectile-with-default-dir (projectile-project-root)
    (tags-query-replace from to delimited
                        '(projectile-current-project-files))))

(define-key projectile-mode-map (kbd "C-c p q")
  'km/projectile-query-and-replace)
