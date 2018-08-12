;;; km-helm.el --- Helm configuration

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

(require 'helm)

;;;###autoload
(defun km/helm-display-buffer ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'display-buffer)))

;;;###autoload
(defun km/helm-visit-in-dired ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-point-file-in-dired)))

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

;;;###autoload
(defun km/helm-display-buffer-below ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (b)
       (display-buffer b '(display-buffer-below-selected))))))

;;;###autoload
(defun km/helm-find-file-below ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (f)
       (select-window
        (display-buffer (find-file-noselect f)
                        '(display-buffer-below-selected)))))))

(defvar km/helm-etags-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'helm-etags-select)
    map))

(define-minor-mode km/helm-etags-mode
  "Override `xref-find-definitions' with `helm-etags-select'."
  :keymap km/helm-etags-mode-map)

(defun km/helm-maybe-override-xref ()
  (when (helm-etags-find-tag-file-directory default-directory)
    (km/helm-etags-mode 1)))

(provide 'km-helm)
;;; km-helm.el ends here
