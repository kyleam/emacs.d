(add-to-list 'load-path "~/src/emacs/snakemake-mode")

(require 'snakemake-mode-autoloads)

(setq snakemake-compile-command-options '("-p"))

;; Although `compile-command' is set when snakemake-mode is derived from
;; Python mode, I need to define it again here because I have a Python
;; mode hook that sets `compile-command', which overides the snakemake
;; version.
(add-hook 'snakemake-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (snakemake-compile-command))))

(defun km/snakemake-compile-project-file (jobs)
  "Run Snakemake to produce project file at point.
The numeric prefix JOBS controls the number of jobs that
Snakemake runs (defaults to 1). If JOBS is zero, perform a dry
run."
  (interactive "p")
  (let* ((fname (or (km/project-filename-at-point)
                    (read-file-name "File: ")))
         (job-flag (if (zerop jobs)
                       " -n "
                     (format " -j%s " jobs)))
         (compile-command (concat (snakemake-compile-command) job-flag
                                  fname))
         (default-directory (projectile-project-root)))
    (call-interactively 'compile)))

(defun km/snakemake-compile-project-rule ()
  "Run `snakemake-compile-rule' from project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively #'snakemake-compile-rule)))

(autoload 'snakemake-compile-command "snakemake-mode")

(after 'init-external
  (define-key km/compile-map "p"
    'km/snakemake-compile-project-file)
  (define-key km/compile-map "b"
    'km/snakemake-compile-project-rule))

(provide 'init-snakemake)
