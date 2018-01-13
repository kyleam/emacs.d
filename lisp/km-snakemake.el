;;; km-snakemake.el --- Snakemake extensions

;; Copyright (C) 2012-2018 Kyle Meyer <kyle@kyleam.com>

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


(require 'compile)
(require 'snakemake)

;;;###autoload
(defun km/snakemake-recompile-no-dryrun ()
  "Recompile last Snakemake call, removing --dryrun."
  (interactive)
  (unless (buffer-live-p compilation-last-buffer)
    (user-error "No previous compile command found"))
  (let ((program snakemake-program))
    (with-current-buffer compilation-last-buffer
      (let ((command (car compilation-arguments)))
        (if (string-prefix-p program command)
            (setf (car compilation-arguments)
                  (replace-regexp-in-string " --dryrun" "" command))
          (user-error "Last compile was not with %s" program)))
      (recompile))))

(provide 'km-snakemake)
;;; km-snakemake.el ends here
