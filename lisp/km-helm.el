;;; km-helm.el --- Helm configuration

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

(require 'helm)

;;;###autoload
(defun km/helm-display-buffer ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'display-buffer)))

;;;###autoload
(defun km/helm-display-file ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (f)
       (display-buffer (find-file-noselect f))))))

(autoload 'org-open-file "org")
;;;###autoload
(defun km/helm-ff-org-open-file ()
  "Run `org-open-file' from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'org-open-file)))

(provide 'km-helm)
;;; km-helm.el ends here
