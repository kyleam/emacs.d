;;; km-ace-link.el --- Extensions for Ace Link

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

(require 'avy)
(require 'dash)
(require 'wid-edit)

(declare-function dired-next-line "dired" (arg))
(defun km/ali--dired-collect-references ()
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (while (< (point) end)
        (--when-let (dired-next-line 1)
          (push it points)))
      (nreverse points))))


(autoload 'org-open-file "org")
(declare-function dired-get-filename "dired"
                  (&optional localp no-error-if-not-filep))
;;;###autoload
(defun km/ace-link-dired ()
  "Ace jump to files in dired buffers."
  (interactive)
  (avy-with km/ace-link-dired
    (setq avy-action
          (lambda (pt)
            (goto-char pt)
            (org-open-file (dired-get-filename))))
    (avy--process
     (km/ali--dired-collect-references)
     #'avy--overlay-post)))

(defun km/ali--widget-collect-references ()
  "Collect the positions of visible widgets in buffer."
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (setq pt (point))
        (while (progn (ignore-errors (widget-forward 1))
                      (> (point) pt))
          (setq pt (point))
          (push (point) candidates))
        (nreverse candidates)))))

(declare-function gnus-summary-widget-forward "gnus-sum" (arg))
;;;###autoload
(defun km/ace-link-widget ()
  "Press a widget that is visible in the current buffer.
This can be used in place of `ace-link-gnus' and has the
advantage of working for gwene buffers in addition to normal mail
buffers because it doesn't rely on the 'gnus-string' text
property."
  (interactive)
  (when (eq major-mode 'gnus-summary-mode)
    (gnus-summary-widget-forward 1))
  (avy-with km/ace-link-widget
    (setq avy-action
          (lambda (pt)
            (goto-char (1+ pt))
            (widget-button-press (point))))
    (avy--process
     (km/ali--widget-collect-references)
     #'avy--overlay-post)))

(provide 'km-ace-link)
;;; km-ace-link.el ends here
