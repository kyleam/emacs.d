;;; km-compile.el --- Compilation extensions

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


(require 'compile)
(require 'dash)

(defvar km/compilation-buffer-name-prefix "compilation: ")

(defun km/compilation-name-by-directory (&optional mode)
  (let ((name (if (and mode (not (equal mode "compilation")))
                  (downcase mode)
                (concat km/compilation-buffer-name-prefix
                        (abbreviate-file-name default-directory)))))
    (concat "*" name "*")))

(defun km/compilation-buffer-p (buffer)
  (with-current-buffer buffer
    (and (derived-mode-p 'compilation-mode)
         (string-prefix-p (concat "*" km/compilation-buffer-name-prefix)
                          (buffer-name)))))

;;;###autoload
(defun km/compile-in-home-dir ()
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively #'compile)))

;;;###autoload
(defun km/compilation-recompile (&optional arg)
  "Recompile buffer.
By default, use `compilation-last-buffer'.  If ARG is 0, get
buffer with name given by `km/compilation-name-by-directory'.
Otherwise, if ARG is non-nil, prompt with buffers from
`km/compilation-buffer-list'."
  (interactive (list (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (with-current-buffer (km/compilation--get-buffer arg)
    (if (derived-mode-p 'occur-mode)
        (revert-buffer)
      (recompile))))

(defun km/compilation-display-buffer (&optional arg)
  "Display compilation buffer.
By default, use `compilation-last-buffer'.  If ARG is 0, get
buffer with name given by `km/compilation-name-by-directory'.
Otherwise, if ARG is non-nil, prompt with buffers from
`km/compilation-buffer-list'."
  (interactive (list (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (display-buffer (km/compilation--get-buffer arg)))

(defun km/compilation--get-buffer (&optional arg)
  (cond
   ((and (not arg)
         (buffer-live-p compilation-last-buffer)
         compilation-last-buffer))
   ((and (numberp arg)
         (= arg 0))
    (get-buffer (km/compilation-name-by-directory)))
   (t
    (let ((cbufs (-map #'buffer-name (km/compilation-buffer-list)))
          buf)
      (cl-case (length cbufs)
        (0 (user-error "No compilation buffers found"))
        (1 (setq buf (car cbufs)))
        (t (setq buf (completing-read "Compilation buffer: " cbufs
                                      nil nil nil nil (car cbufs)))))
      buf))))

(defun km/compilation-buffer-list ()
  (-filter #'km/compilation-buffer-p (buffer-list)))

(provide 'km-compile)
;;; km-compile.el ends here
