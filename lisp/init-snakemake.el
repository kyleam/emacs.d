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

(defun km/snakemake-compile-project-file-at-point (arg)
  "Run Snakemake to produce project file at point.
With prefix ARG, use file name as is, without trying to append
the project's path."
  (interactive "P")
  (let* ((fname (if arg
                    (thing-at-point 'filename)
                  (km/project-filename-at-point)))
         (compile-command (concat (snakemake-compile-command) " "
                                  fname))
         (default-directory (projectile-project-root)))
    (call-interactively 'compile)))

(autoload 'snakemake-compile-command "snakemake-mode")

(after 'init-external
  (define-key km/compile-map "p"
    'km/snakemake-compile-project-file-at-point))

(provide 'init-snakemake)
