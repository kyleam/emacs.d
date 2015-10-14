(require 'projectile)
(require 'helm-projectile)

(setq projectile-find-dir-includes-top-level t
      projectile-completion-system 'helm
      projectile-use-git-grep t)

(projectile-register-project-type 'snakemake '("Snakefile") "snakemake -p" "")

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

(defun km/projectile-ignore-directory-p (name)
  (or (file-remote-p name)
      (string-prefix-p "/tmp/" name)))

(setq projectile-ignored-project-function #'km/projectile-ignore-directory-p)

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
  (let* ((el (and (derived-mode-p 'org-mode)
                  (org-element-lineage (org-element-context) '(link) t)))
         (fname (or (and (eq (org-element-type el) 'link)
                         (org-element-property :path el))
                    (thing-at-point 'filename))))
    (when fname
      (file-relative-name fname (projectile-project-root)))))

(defun km/projectile-copy-project-filename-as-kill ()
  "Copy name of project file.
If point is on a file, copy this as the file name.  Otherwise,
use the name of the current file."
  (interactive)
  (-when-let (fname (or (km/project-filename-at-point)
                        (and buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (projectile-project-root)))))
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
Like `projectile-kill-buffers', but
- Before killing buffers, delete any saved thing for the project.
- Don't ask for confirmation to kill project buffers (but
  `kill-buffer' will still ask when killing a modified buffer)."
  (interactive)
  (km/projectile-save-thing ?d)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t)))
    (projectile-kill-buffers)))

;; I'm redefining a lot of bindings, so just set the whole map here to
;; have everything in one place.

(setq projectile-command-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "4 b") 'projectile-switch-to-buffer-other-window)
        (define-key map (kbd "4 C-o") 'projectile-display-buffer)
        (define-key map (kbd "4 d") 'projectile-find-dir-other-window)
        (define-key map (kbd "4 f") 'projectile-find-file-other-window)
        (define-key map (kbd "4 v") 'km/projectile-view-file-other-window)
        (define-key map (kbd ".") 'km/projectile-copy-project-filename-as-kill)
        (define-key map "!" 'projectile-run-shell-command-in-root)
        (define-key map "&" 'projectile-run-async-shell-command-in-root)
        (define-key map "b" 'helm-projectile-switch-to-buffer)
        (define-key map "c" 'projectile-compile-project)
        (define-key map "d" 'helm-projectile-find-dir)
        (define-key map "e" 'km/projectile-restore-thing)
        (define-key map "f" 'helm-projectile-find-file)
        (define-key map "F" 'helm-projectile-find-file-in-known-projects)
        (define-key map "g" 'projectile-vc)
        (define-key map "i" 'projectile-ibuffer)
        (define-key map "I" 'projectile-invalidate-cache)
        (define-key map "k" 'km/projectile-kill-buffers)
        (define-key map "l" 'projectile-project-buffers-other-buffer)
        (define-key map "m" 'projectile-commander)
        (define-key map "o" 'projectile-multi-occur)
        (define-key map "p" 'helm-projectile-switch-project)
        (define-key map "q" 'projectile-replace)
        (define-key map "r" 'helm-projectile-recentf)
        (define-key map "s" 'projectile-grep)
        (define-key map "v" 'km/projectile-view-file)
        (define-key map "w" 'km/projectile-save-thing)
        map))
(fset 'projectile-command-map projectile-command-map)

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

;; Like `projectile-command-map', I'm redefining a lot of bindings, so
;; unset pre-defined methods and define everyting here.

(setq projectile-commander-methods nil)

(def-projectile-commander-method ?b
  "Find project buffer."
  (call-interactively 'helm-projectile-switch-to-buffer))

(def-projectile-commander-method ?c
  "Run project compilation command."
  (call-interactively 'projectile-compile-project))

(def-projectile-commander-method ?d
  "Find directory in project."
  (helm-projectile-find-dir))

(def-projectile-commander-method ?D
  "Find a project directory in other window."
  (call-interactively 'projectile-find-dir-other-window))

(def-projectile-commander-method ?e
  "Restore saved thing."
  (km/projectile-restore-thing))

(def-projectile-commander-method ?f
  "Open project file."
  (helm-projectile-find-file))

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

(def-projectile-commander-method ?o
  "Display a project buffer in other window."
  (call-interactively 'projectile-display-buffer))

(def-projectile-commander-method ?O
  "Run multi-occur on project buffers."
  (projectile-multi-occur))

(def-projectile-commander-method ?p
  "Switch project."
  (helm-projectile-switch-project))

(def-projectile-commander-method ?r
  "Find recently visited file in project."
  (projectile-recentf))

(def-projectile-commander-method ?s
  "Run grep on project."
  (call-interactively #'projectile-grep))

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
