;;; km-snakemake.el --- Extensions for Snakemake mode

;; Copyright (C) 2012-2016 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/emacs.d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'projectile)
(require 'km-projectile)
(require 'snakemake-mode)

;;;###autoload
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

;;;###autoload
(defun km/snakemake-compile-project-rule ()
  "Run `snakemake-compile-rule' from project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively #'snakemake-compile-rule)))

(provide 'km-snakemake)
;;; km-snakemake.el ends here
