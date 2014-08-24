(require 'snakemake-mode-autoloads)

(setq snakemake-compile-command-options '("-p"))

;; Although `compile-command' is set when snakemake-mode is derived from
;; Python mode, I need to define it again here because I have a Python
;; mode hook that sets `compile-command', which overides the snakemake
;; version.
(add-hook 'snakemake-mode-hook
          '(lambda ()
             (set (make-local-variable 'compile-command)
                  (snakemake-compile-command))))

(defun km/snakemake-compile-project-file-at-point ()
  "Run Snakemake to produce project file at point."
  (interactive)
  (let ((compile-command (concat (snakemake-compile-command) " "
                                 (km/project-filename-at-point)))
        (default-directory (projectile-project-root)))
    (call-interactively 'compile)))

(autoload 'snakemake-compile-command "snakemake-mode")

(provide 'init-snakemake)
