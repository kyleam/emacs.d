(add-to-list 'load-path "~/src/emacs/snakemake-mode/")
(require 'snakemake-mode-autoloads)

(autoload 'snakemake-compile-command "snakemake-mode")

(setq snakemake-compile-command-options '("-p"))

;; Although `compile-command' and `imenu-create-index-function' are
;; set when snakemake-mode is derived from Python mode, I need to
;; define them again here because I have a Python mode hook overrides
;; the Python versions.
(add-hook 'snakemake-mode-hook #'km/snakemake-set-local-vars)

(defun km/snakemake-set-local-vars ()
  (set (make-local-variable 'compile-command)
       (snakemake-compile-command))
  (set (make-local-variable 'imenu-create-index-function)
       #'snakemake-imenu-create-index))

(defun km/snakemake-compile-project-file (jobs)
  "Run Snakemake to produce project file at point.
The numeric prefix JOBS controls the number of jobs that
Snakemake runs (defaults to 1).  If JOBS is zero, perform a dry
run.  If JOBS is negative, just touch the output files."
  (interactive "p")
  (let* ((fname (or (km/project-filename-at-point)
                    (read-file-name "File: ")))
         (job-flag (cond
                    ((> jobs 0) (format " -j%s " jobs))
                    ((zerop jobs) " -n ")
                    (t " -t ")))
         (compile-command (concat (snakemake-compile-command) job-flag
                                  fname))
         (default-directory (projectile-project-root)))
    (call-interactively 'compile)))

(defun km/snakemake-compile-project-rule ()
  "Run `snakemake-compile-rule' from project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively #'snakemake-compile-rule)))

(after 'init-external
  (define-key km/compile-map "b"
    'km/snakemake-compile-project-rule)
  (define-key km/compile-map "p"
    'km/snakemake-compile-project-file))

(after 'dired
  (define-key dired-mode-map "b" 'km/snakemake-compile-project-file))

(provide 'init-snakemake)
