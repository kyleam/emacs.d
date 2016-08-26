;;; km-projectile.el --- Projectile configuration

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

(require 'dired)
(require 'projectile)
(require 'org-element)

;;;###autoload
(defun km/projectile-switch-project (&optional arg)
  "Switch to a project.

Like `projectile-switch-project', but instead of calling
`projectile-commander' when a prefix argument ARG is given, save
something for the current project before switching.

`projectile-switch-project-action' is set to
`km/projectile-maybe-restore-thing'.  If the thing saved for the
destination project is the the window configuration, this may not
end up in the project if the buffers are now dead."
  (interactive "P")
  (when arg (call-interactively #'km/projectile-save-thing))
  (let ((projectile-switch-project-action 'km/projectile-maybe-restore-thing))
    (projectile-switch-project)))

(declare-function km/open-external-terminal "km-shell")
;;;###autoload
(defun km/projectile-open-external-terminal-in-root ()
  "Run `km/open-external-terminal' in project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (km/open-external-terminal)))

;;;###autoload
(defun km/projectile-view-file ()
  "View project file.
Interactive arguments are processed according to
`projectile-find-file'."
  (interactive)
  (call-interactively #'projectile-find-file)
  (view-mode 1))

;;;###autoload
(defun km/projectile-view-file-other-window ()
  "View project file in other window.
Interactive arguments are processed according to
`projectile-find-file-other-window'."
  (interactive)
  (call-interactively 'projectile-find-file-other-window)
  (view-mode 1))

(defun km/project-filename-at-point ()
  "Return file name relative to `projectile-project-root'.
In the case of multiple files marked in Dired, return the file
names separated by a space."
  (let* ((el (and (derived-mode-p 'org-mode)
                  (org-element-lineage (org-element-context) '(link) t)))
         (fname (or (and (eq (org-element-type el) 'link)
                         (org-element-property :path el))
                    (and (derived-mode-p 'dired-mode)
                         (dired-get-marked-files 'nodir nil))
                    (thing-at-point 'filename))))
    (when fname
      (mapconcat
       `(lambda (f) (file-relative-name f ,(projectile-project-root)))
       (if (listp fname) fname (list fname))
       " "))))

;;;###autoload
(defun km/projectile-copy-project-filename-as-kill ()
  "Copy name of project file.
If point is on a file, copy this as the file name.  Otherwise,
use the name of the current file."
  (interactive)
  (-when-let (fname (or (km/project-filename-at-point)
                        (and buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (projectile-project-root)))))
    (if (eq last-command 'kill-region)
        (kill-append fname nil)
      (kill-new fname))
    (message "%s" fname)))

(defvar km/projectile-project-saved-thing nil
  "Property list of saved thing for projects.
The keys are project roots (strings), so use `lax-plist-put' and
`lax-plist-get'.")

;;;###autoload
(defun km/projectile-save-thing (thing)
  "Save thing for current project.

Thing is a character representing
-  . point marker
- (b)uffer
- (f)ile
- (w)indow configuration

- (d)elete saved thing"
  (interactive (list
                (let ((letters '(?. ?b ?f ?w ?d)))
                  (read-char-choice (concat "Save [" letters "]: ")
                                    letters))))
  (let ((value (cl-case thing
                 (?.
                  (point-marker))
                 (?b
                  (current-buffer))
                 (?f
                  (buffer-file-name))
                 (?w
                  (current-window-configuration))
                 (?d nil))))
    (setq km/projectile-project-saved-thing
          (lax-plist-put km/projectile-project-saved-thing
                         (projectile-project-root)
                         (cons thing value)))))

(defun km/projectile-restore-thing ()
  "Restore saved thing for current project.
Return nil if there is no thing saved for the current project."
  (interactive)
  (-when-let* ((thing-value (lax-plist-get km/projectile-project-saved-thing
                                           (projectile-project-root)))
               (thing (car thing-value))
               (value (cdr thing-value)))
    (cl-case thing
      (?.
       (-if-let (buf (marker-buffer value))
           (progn (switch-to-buffer buf)
                  (goto-char value))
          (user-error "Buffer no longer exists")))
      (?b
       (if (buffer-live-p value)
           (switch-to-buffer value)
         (user-error "Buffer no longer exists")))
      (?f
       (find-file value))
      (?w
       (set-window-configuration value)))
    t))

(defvar km/projectile-switch-fallback 'projectile-commander)

(defun km/projectile-maybe-restore-thing ()
  "Try to restore thing for current project.
If there is nothing ot restore, call
`km/projectile-switch-fallback'."
  (or (km/projectile-restore-thing)
      (funcall km/projectile-switch-fallback)))

;;;###autoload
(defun km/projectile-kill-buffers ()
  "Kill all project buffers.
Like `projectile-kill-buffers', but
- Before killing buffers, delete any saved thing for the project.
- Don't ask for confirmation to kill project buffers (but
  `kill-buffer' will still ask when killing a modified buffer)."
  (interactive)
  (km/projectile-save-thing ?d)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t)))
    (projectile-kill-buffers)))

;;;###autoload
(defun km/projectile-kill-other-buffers ()
  "Kill all project buffers except the current one."
  (interactive)
  (let ((cbuf (current-buffer)))
    (mapc #'kill-buffer
          (cl-remove-if
           (lambda (b) (or (buffer-base-buffer b) (eq cbuf b)))
           (projectile-project-buffers)))))

(provide 'km-projectile)
;;; km-projectile.el ends here
