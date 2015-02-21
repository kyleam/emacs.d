(require 'projectile)

(setq projectile-find-dir-includes-top-level t
      projectile-use-git-grep t)

(defun km/projectile-switch-project (&optional arg)
  "Switch to a project.

Like `projectile-switch-project', but instead of calling
`projectile-commander' when a prefix argument ARG is given, save
something for the current project before switching.

`projectile-switch-project-action' is set to
`km/projectile-maybe-restore-thing'.  If the thing saved for the
destination project is the the window configuration, this may not
end up in the project if the buffers are now dead."
  (interactive "P")
  (when arg (call-interactively #'km/projectile-save-thing))
  (let ((projectile-switch-project-action 'km/projectile-maybe-restore-thing))
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

(defvar km/projectile-project-saved-thing nil
  "Property list of saved thing for projects.
The keys are project roots (strings), so use `lax-plist-put' and
`lax-plist-get'.")

(defun km/projectile-save-thing (thing)
  "Save thing for current project.

Thing is a character representing
-  . point marker
- (b)uffer
- (f)ile
- (w)indow configuration

- (d)elete saved thing"
  (interactive (list
                (let ((letters '(?. ?b ?f ?w ?d)))
                  (read-char-choice (concat "Save [" letters "]: ")
                                    letters))))
  (let ((value (cl-case thing
                 (?.
                  (point-marker))
                 (?b
                  (current-buffer))
                 (?f
                  (buffer-file-name))
                 (?w
                  (current-window-configuration))
                 (?d nil))))
    (setq km/projectile-project-saved-thing
          (lax-plist-put km/projectile-project-saved-thing
                         (projectile-project-root)
                         (cons thing value)))))

(defun km/projectile-restore-thing ()
  "Restore saved thing for current project.
Return nil if there is no thing saved for the current project."
  (interactive)
  (-when-let* ((thing-value (lax-plist-get km/projectile-project-saved-thing
                                           (projectile-project-root)))
               (thing (car thing-value))
               (value (cdr thing-value)))
    (cl-case thing
      (?.
       (-if-let (buf (marker-buffer value))
           (progn (switch-to-buffer buf)
                  (goto-char value))
          (user-error "Buffer no longer exists")))
      (?b
       (if (buffer-live-p value)
           (switch-to-buffer value)
         (user-error "Buffer no longer exists")))
      (?f
       (find-file value))
      (?w
       (set-window-configuration value)))
    t))

(defvar km/projectile-switch-fallback 'projectile-commander)

(defun km/projectile-maybe-restore-thing ()
  "Try to restore thing for current project.
If there is nothing ot restore, call
`km/projectile-switch-fallback'."
  (or (km/projectile-restore-thing)
      (funcall km/projectile-switch-fallback)))

(defun km/projectile-kill-buffers ()
  "Kill all project buffers.
Before running `projectile-kill-buffers', delete any saved thing
for the project."
  (interactive)
  (km/projectile-save-thing ?d)
  (projectile-kill-buffers))

(define-key projectile-command-map (kbd "4 v")
  'km/projectile-view-file-other-window)

(define-key projectile-command-map "."
  'km/projectile-copy-project-filename-as-kill)
(define-key projectile-command-map "e" 'km/projectile-restore-thing)
;; This overrides `projectile-find-file-dwim'.
(define-key projectile-command-map "g" 'projectile-vc)
;; Swap `projectile-invalidate-cache' and `projectile-ibuffer'.
(define-key projectile-command-map "I" 'projectile-invalidate-cache)
(define-key projectile-command-map "i" 'projectile-ibuffer)
(define-key projectile-command-map "k" 'km/projectile-kill-buffers)
(define-key projectile-command-map "q" 'projectile-replace)
;; This overrides `projectile-replace', which is now on 'q'.
(define-key projectile-command-map "r" 'projectile-recentf)
;; This overrides Projectile's general search prefix.
(define-key projectile-command-map "s" 'projectile-grep)
;; This overrides `projectile-vc', which is now on 'g'.
(define-key projectile-command-map "v" 'km/projectile-view-file)
(define-key projectile-command-map "w" 'km/projectile-save-thing)

(key-chord-define-global "jq" 'projectile-commander)
(key-chord-define-global "gp" 'km/projectile-switch-project)

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
(define-key km/projectile-ctl-x-4-map "t"
  'projectile-find-implementation-or-test-other-window)
(define-key km/projectile-ctl-x-4-map "v"
  'km/projectile-view-file-other-window)


;;; Commander methods

(def-projectile-commander-method ?B
  "Find project buffer in other window."
  (call-interactively 'projectile-switch-to-buffer-other-window))

(def-projectile-commander-method ?c
  "Run project compilation command."
  (call-interactively 'projectile-compile-project))

(def-projectile-commander-method ?D
  "Find a project directory in other window."
  (call-interactively 'projectile-find-dir-other-window))

(def-projectile-commander-method ?e
  "Restore saved thing."
  (km/projectile-restore-thing))

(def-projectile-commander-method ?F
  "Find project file in other window."
  (call-interactively 'projectile-find-file-other-window))

(def-projectile-commander-method ?g
  "Open project root in vc-dir or magit."
  (projectile-vc))

(def-projectile-commander-method ?i
  "Open an IBuffer window showing all buffers in the current project."
  (call-interactively 'projectile-ibuffer))

(def-projectile-commander-method ?k
  "Kill all project buffers."
  (call-interactively 'km/projectile-kill-buffers))

(def-projectile-commander-method ?O
  "Display a project buffer in other window."
  (call-interactively 'projectile-display-buffer))

(def-projectile-commander-method ?r
  "Find recently visited file in project."
  (projectile-recentf))

(def-projectile-commander-method ?s
  "Run grep on project."
  (call-interactively #'projectile-grep))

(def-projectile-commander-method ?t
  "Open project root in dired."
  (projectile-dired))

(def-projectile-commander-method ?v
  "View project file."
  (km/projectile-view-file))

(def-projectile-commander-method ?V
  "View project file in other window."
  (km/projectile-view-file-other-window))

(def-projectile-commander-method ?w
  "Save thing."
  (call-interactively #'km/projectile-save-thing))

(projectile-global-mode)

(provide 'init-projectile)
