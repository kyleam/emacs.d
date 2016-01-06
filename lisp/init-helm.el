;;; init-helm.el --- Helm configuration

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
(require 'helm-config)

(setq helm-move-to-line-cycle-in-source t
      helm-ff-newfile-prompt-p nil
      helm-ff-file-name-history-use-recentf t
      helm-ff-skip-boring-files t)

(defun km/helm-display-buffer ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action #'display-buffer)))

(defun km/helm-display-file ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action
     (lambda (f)
       (display-buffer (find-file-noselect f))))))

(defun km/helm-ff-org-open-file ()
  "Run `org-open-file' from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action #'org-open-file)))

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x c") nil)

(after 'helm-files
  (define-key helm-find-files-map (kbd "C-c x") 'km/helm-ff-org-open-file)
  (define-key helm-generic-files-map (kbd "C-c x") 'km/helm-ff-org-open-file)
  ;; Overrides `helm-ff-run-switch-other-frame'.
  (define-key helm-find-files-map (kbd "C-c C-o") 'km/helm-display-file)
  (define-key helm-generic-files-map (kbd "C-c C-o") 'km/helm-display-file)
  ;; Overrides `helm-buffer-switch-other-frame'.
  (define-key helm-buffer-map (kbd "C-c C-o") 'km/helm-display-buffer))

(key-chord-define-global "jc" 'helm-find-files)
(key-chord-define-global "jt" 'helm-mini)
(key-chord-define-global "kx" 'helm-M-x)

(define-key search-map "k" 'helm-swoop)

(global-set-key (kbd "C-h a") 'helm-apropos)

(helm-mode 1)

(provide 'init-helm)
;;; init-helm.el ends here
