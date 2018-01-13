;;; km-org-agenda.el --- Org mode agenda extensions

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

(require 'org-agenda)
(require 'km-org)

(defvar km/org-agenda-file-directory nil)

(defun km/org-agenda-cd-and-read-dir-locals ()
  (unless (get 'org-agenda-files 'org-restrict)
    (setq default-directory (expand-file-name "~/notes/"))
    (hack-local-variables)))

(defun km/org-agenda-store-current-span ()
    "Store the current span value in `org-agenda-span'.
This allows the view to persist when the agenda buffer is
killed."
    (when org-agenda-current-span
      (setq org-agenda-span org-agenda-current-span)))

;;;###autoload
(defun km/org-agenda-add-or-remove-file (file)
  "Add or remove link to FILE in `km/org-agenda-file-directory'.
If a link for FILE does not exist, create it. Otherwise, remove
it. Like `org-agenda-file-to-front', this results in FILE being
displayed in the agenda."
  (interactive (list (cl-case major-mode
                       (org-mode (buffer-file-name))
                       (dired-mode (dired-get-filename))
                       (org-agenda-mode (ignore-errors (save-window-excursion
                                                         (org-agenda-goto)
                                                         (buffer-file-name))))
                       (t (read-file-name "Link file: ")))))
  (let ((agenda-file (expand-file-name (file-name-nondirectory file)
                                       km/org-agenda-file-directory)))
    (if (file-equal-p (file-truename agenda-file) file)
        (progn
          (when (called-interactively-p) (message "Deleting %s" agenda-file))
          (delete-file agenda-file))
      (when (called-interactively-p) (message "Adding %s" agenda-file))
      (make-symbolic-link file agenda-file))))

;;;###autoload
(defun km/org-open-default-notes-file-inbox ()
  "Open \"Inbox\" heading of `org-default-notes-file'."
  (interactive)
  (find-file org-default-notes-file)
  (goto-char (org-find-exact-headline-in-buffer "Inbox" nil t))
  (recenter-top-bottom 0)
  (show-children))

;;;###autoload
(defun km/org-goto-agenda-heading ()
  "Jump to heading in agenda files."
  (interactive)
  (let ((org-refile-targets
         '((org-agenda-files :maxlevel . 3)
           (org-agenda-text-search-extra-files :maxlevel . 3)))
        (org-refile-use-outline-path t))
    (org-refile '(4))))

(defun km/org-delete-subtree ()
  (org-back-to-heading t)
  (delete-region
   (point)
   (org-element-property :end (org-element-at-point))))

(defun km/org-agenda-delete-subtree ()
  (interactive)
  (org-agenda-archive-with #'km/org-delete-subtree))

;;;###autoload
(defun km/org-agenda-set-restriction-lock (&optional type)
  "Call `org-agenda-set-restriction-lock' with flipped C-u meaning."
  (interactive "P")
  (org-agenda-set-restriction-lock
   (cond ((equal type '(4)) nil)
         (type)
         (t '(4)))))

(defun km/org-agenda-refile-dwim ()
  "Rebind `org-refile-targets' if next window is an Org buffer.
A target is determined by `km/org-refile-dwim-target-file'."
  (interactive)
  (let* ((dwim-target (km/org-refile-dwim-target-file))
         (org-refile-targets (if dwim-target
                                 `((nil
                                    :maxlevel . ,km/org-refile-dwim-maxlevel)
                                   (dwim-target
                                    :maxlevel . ,km/org-refile-dwim-maxlevel))
                               org-refile-targets)))
    (call-interactively #'org-agenda-refile)))

(defun km/org-agenda-reschedule-by-days ()
  (interactive)
  (org-agenda-schedule
   nil
   (concat "+" (read-string "Days from now: ") "d")))

(provide 'km-org-agenda)
;;; km-org-agenda.el ends here
