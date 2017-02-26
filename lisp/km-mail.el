;;; km-mail.el --- Mail-related extensions

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
(require 'notmuch)

(require 'km-util)

;;;###autoload
(defun km/notmuch-show-open-github-patch ()
  "Open patch from GitHub email."
  (interactive)
  (with-current-notmuch-show-message
   (km/open-github-patch (current-buffer))))


;;; Mail sync

(defun mail-sync-log-buffer (buf _)
  (let ((bstring (with-current-buffer buf
                   (buffer-string))))
    (with-current-buffer (get-buffer-create "*mail-sync-log*")
      (goto-char (point-max))
      (insert "\n\n\n")
      (insert bstring))))

(defvar mail-sync-calling-buffer nil)
(defun mail-sync-refresh-caller (_ exit)
  (when (equal exit "finished\n")
    (when (and mail-sync-calling-buffer
               (buffer-live-p mail-sync-calling-buffer))
      (with-current-buffer mail-sync-calling-buffer
        (notmuch-refresh-this-buffer))))
  (setq mail-sync-calling-buffer nil))

;;;###autoload
(define-compilation-mode mail-sync-mode "Mail-sync"
  "Sync mail, logging output to *mail-sync-log*."
  (set (make-local-variable 'compilation-finish-functions)
       '(mail-sync-log-buffer mail-sync-refresh-caller)))

;;;###autoload
(defun km/notmuch-sync-mail (&optional cmd-append)
  (interactive (list (and current-prefix-arg
                          (read-string "sync-mail args: "))))
  (setq mail-sync-calling-buffer (current-buffer))
  (let ((default-directory (expand-file-name "~/"))
        (display-buffer-overriding-action
         '(display-buffer-below-selected)))
    (compilation-start (concat "sync-mail"
                               (and cmd-append " ")
                               cmd-append)
                       'mail-sync-mode)))

;;;###autoload
(defun km/notmuch-sync-mail-fast ()
  (interactive)
  (km/notmuch-sync-mail "--fast"))

(provide 'km-mail)
;;; km-mail.el ends here
