;;; km-compile.el --- Compilation extensions

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

(defvar km/compilation-last-buffer nil
  "Like `compilation-last-buffer' but don't set in derived modes.")

(defun km/store-compilation-last-buffer (_proc)
  (when (eq major-mode 'compilation-mode)
    (setq km/compilation-last-buffer (current-buffer))))

(add-hook 'compilation-start-hook #'km/store-compilation-last-buffer)

;;;###autoload
(defun km/compile-in-home-dir ()
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively #'compile)))

;;;###autoload
(defun km/compilation-recompile (&optional arg)
  "Recompile buffer.
By default, use `km/compilation-last-buffer'.  If ARG is 0, get
buffer with name given by `km/compilation-name-by-directory'.
Otherwise, if ARG is non-nil, prompt with buffers from
`km/compilation-buffer-list'."
  (interactive (list (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (with-current-buffer (km/compilation--get-buffer arg)
    (recompile)))

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
         (buffer-live-p km/compilation-last-buffer)
         km/compilation-last-buffer))
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

(defun km/compile-make-pdf-target (command)
  (and (string-match (rx string-start
                         (zero-or-one "snake") "make"
                         (zero-or-more not-newline) space
                         (group (one-or-more (not space)) ".pdf")
                         (zero-or-more space)
                         string-end)
                     command)
       (match-string 1 command)))

(defun km/compile-latexmk-pdf-target (command)
  (and (string-match (rx string-start
                         "latexmk"
                         (zero-or-more not-newline) space
                         (group (one-or-more (not space)))
                         (zero-or-one ".tex")
                         (zero-or-more space)
                         string-end)
                     command)
       (concat (match-string 1 command) ".pdf")))

(defvar km/compile-pdf-functions '(km/compile-make-pdf-target
                                   km/compile-latexmk-pdf-target))

(defun km/compile-check-pdf (buf exit)
  (when (equal exit "finished\n")
    (with-current-buffer buf
      (let* ((cmd (car compilation-arguments))
             (pdf-file (run-hook-with-args-until-success
                        'km/compile-pdf-functions cmd)))
        (when pdf-file
          (call-process "xdotool" nil nil nil
                        "search" "--all" "--class" "--name"
                        (concat "^mupdf|" (file-name-nondirectory pdf-file))
                        "key" "--window" "%@" "r"))))))

(add-to-list 'compilation-finish-functions #'km/compile-check-pdf)

(provide 'km-compile)
;;; km-compile.el ends here
