;;; init-dired.el --- Dired configuration

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

(require 'dired-x)

(put 'dired-find-alternate-file 'disabled nil)

;; .git is present as part of `dired-omit-extensions', but this seems to
;; only be taken into account if a non-exension part exists.
(setq dired-omit-files
      (concat dired-omit-files
              "\\|^\\.git$\\|^\\.gitignore$"
              "\\|^__pycache__$\\|^\\.snakemake$"))

(defvar km/latex-omit-extensions '(".aux"
                                   ".fdb_latexmk"
                                   ".fls"
                                   ".log"
                                   ".nav"
                                   ".out"
                                   ".snm")
  "Intermediate LaTeX files")

(setq dired-omit-extensions
      (append dired-omit-extensions km/latex-omit-extensions))

(setq-default dired-omit-mode t)
(setq dired-dwim-target t
      dired-listing-switches "-alht")

(setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "zathura")))

(setq dired-recursive-copies t
      dired-recursive-deletes t)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(defun km/dired-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (km/dired-completing-buffer)))

(defun km/dired-switch-to-buffer-other-window ()
  (interactive)
  (pop-to-buffer (km/dired-completing-buffer)))

(defun km/dired-completing-buffer ()
  (completing-read "Dired buffer: "
                   (-map 'buffer-name (km/dired-buffer-list))))

(defun km/dired-buffer-list ()
  (--filter (with-current-buffer it
              (derived-mode-p 'dired-mode))
            (buffer-list)))

(defun km/org-open-dired-marked-files (&optional arg)
  "Open marked files (or next ARG) with `org-open-file'."
  (interactive "p")
  (setq arg (and current-prefix-arg arg))
  (let* ((files (dired-get-marked-files nil arg))
         (num-files (length files)))
    (when (or (< num-files 5)
              (yes-or-no-p (format "Open %s files?" num-files)))
      (dolist (f files) (org-open-file f)))))

(defun km/dired-view-file-other-window ()
  "In Dired, view this file in another window."
  (interactive)
  (view-file-other-window (dired-get-file-for-visit)))

(defun km/dired-copy-and-edit ()
  "Copy file and enter `wdired-mode' for completing rename."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Must be in a Dired buffer"))
  (let* ((fname (dired-get-filename))
         (flag "---copy---")
         (new-fname (concat fname flag)))
    (when (file-directory-p fname)
      (user-error "File cannot be directory"))
    (copy-file fname new-fname)
    (dired-revert)
    (wdired-change-to-wdired-mode)
    (goto-char (point-min))
    (re-search-forward (format "%s\\(%s\\)"
                               (file-name-nondirectory fname)
                               flag))
    (replace-match "" t nil nil 1)))

;; This overrides the binding for `list-directory'.
(global-set-key (kbd "C-x C-d") 'km/dired-switch-to-buffer)
(define-key dired-mode-map "c" 'dired-do-copy)
(define-key dired-mode-map "C" 'km/dired-copy-and-edit)
;; This overrides `dired-do-run-mail'.
(define-key dired-mode-map "V" 'km/dired-view-file-other-window)

(define-key ctl-x-4-map "D" 'km/dired-switch-to-buffer-other-window)

(define-prefix-command 'km/dired-prefix-map)
(define-key dired-mode-map (kbd "C-c m") 'km/dired-prefix-map)

(after 'org
  ;; This overrides `dired-find-file', which is also bound to "f".
  (define-key dired-mode-map "e" 'km/org-open-dired-marked-files))


;;; Dired Narrow

(define-key dired-mode-map "/" 'dired-narrow)

(define-prefix-command 'km/dired-narrow-prefix-map)
(define-key km/dired-prefix-map "n" 'km/dired-narrow-prefix-map)

(define-key km/dired-narrow-prefix-map "f" 'dired-narrow-fuzzy)
(define-key km/dired-narrow-prefix-map "n" 'dired-narrow)
(define-key km/dired-narrow-prefix-map "r" 'dired-narrow-regexp)


;;; Dired Subtree

(define-prefix-command 'km/dired-subtree-prefix-map)
(define-key km/dired-prefix-map "s" 'km/dired-subtree-prefix-map)

(define-key km/dired-subtree-prefix-map "@" 'dired-subtree-mark-subtree)
(define-key km/dired-subtree-prefix-map "." 'dired-subtree-unmark-subtree)
(define-key km/dired-subtree-prefix-map "<" 'dired-subtree-beginning)
(define-key km/dired-subtree-prefix-map ">" 'dired-subtree-end)
(define-key km/dired-subtree-prefix-map "g" 'dired-subtree-revert)
(define-key km/dired-subtree-prefix-map "d" 'dired-subtree-down)
(define-key km/dired-subtree-prefix-map "i" 'dired-subtree-insert)
(define-key km/dired-subtree-prefix-map "n" 'dired-subtree-next-sibling)
(define-key km/dired-subtree-prefix-map "p" 'dired-subtree-previous-sibling)
(define-key km/dired-subtree-prefix-map "r" 'dired-subtree-remove)
(define-key km/dired-subtree-prefix-map "s" 'dired-subtree-narrow)
(define-key km/dired-subtree-prefix-map "u" 'dired-subtree-up)


;;; Copying file names

(defun km/dired-copy-project-filename-as-kill ()
  "Copy names of marked project files into kill ring.
This is similar to `dired-copy-filename-as-kill', but the leading
path is always relative to `projectile-project-root'."
  (interactive)
  (km/dired-copy-filename-relative-to-directory
   (projectile-project-root)))

(defun km/dired-copy-relative-filename-as-kill (&optional arg)
  "Copy names of marked (or next ARG) files into kill ring.
This is similar to `dired-copy-filename-as-kill', but the leading
path is always relative to the `default-directory' of the other
window."
  (interactive "p")
  (setq arg (and current-prefix-arg arg))
  (km/dired-copy-filename-relative-to-directory
   (km/other-default-directory) arg))

(defun km/dired-copy-filename-relative-to-directory (directory &optional arg)
  "Like `dired-copy-filename-as-kill', but the filename is always
relative to DIRECTORY."
  (let* ((string
          (mapconcat #'identity
                     (mapcar (lambda (f) (file-relative-name f directory))
                             (dired-get-marked-files t arg))
                     " ")))
    (if (eq last-command 'kill-region)
        (kill-append string nil)
      (kill-new string))
    (message "%s" string)))

(defun km/other-default-directory ()
  "Get `default-directory' for result of `(other-window 1)'."
  (save-window-excursion
    (other-window 1)
    default-directory))

(define-prefix-command 'km/dired-copy-filename-map)
;; This overrides the default binding for `dired-copy-filename-as-kill'.
(define-key dired-mode-map "w" 'km/dired-copy-filename-map)

(after 'projectile
  (define-key km/dired-copy-filename-map "p"
    'km/dired-copy-project-filename-as-kill))
(define-key km/dired-copy-filename-map "o" 'km/dired-copy-relative-filename-as-kill)
(define-key km/dired-copy-filename-map "w" 'dired-copy-filename-as-kill)

(provide 'init-dired)
;;; init-dired.el ends here
