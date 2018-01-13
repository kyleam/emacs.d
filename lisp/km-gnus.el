;;; km-mail.el --- Gnus-related extensions

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

(require 'dash)
(require 'projectile)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-topic)
(require 'gnus-sum)
(require 'shr)

(require 'km-util)

;; http://www.emacswiki.org/emacs/GnusSync
(defun km/gnus-grace-exit-before-kill-emacs ()
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (let ((noninteractive t))
        (gnus-group-exit))))

(defun km/gnus-group-sort-by-topic (info1 info2)
  "Sort alphabetically by group topic.
This allows groups to be ordered by topics even when topic mode
is off."
  (require 'gnus-topic)
  (string< (gnus-group-topic (gnus-info-group info1))
           (gnus-group-topic (gnus-info-group info2))))

(defun km/gnus-summary-set-current-article ()
  (unless gnus-summary-buffer
    (user-error "No summary buffer"))
  (with-current-buffer gnus-summary-buffer
    (save-window-excursion (gnus-summary-select-article))))

(defun km/gnus--last-message-link ()
  (with-current-buffer gnus-article-buffer
    (save-excursion
      (goto-char (point-max))
      (widget-forward -1)
      (--when-let (or (get-text-property (point) 'gnus-string)
                      (get-text-property (point) 'shr-url))
        (kill-new it)))))

(defun km/gnus--gmane-link (&optional perma)
  (with-current-buffer gnus-original-article-buffer
    (-when-let* ((blink (message-field-value "Archived-At"))
                 (link (or (and (string-match "\\`<\\(.*\\)>\\'" blink)
                                (match-string 1 blink))
                           blink)))
      (if perma
          link
        (replace-regexp-in-string "\\`http://permalink\.gmane\.org/"
                                    "http://thread.gmane.org/"
                                    link)))))

(defun km/gnus-copy-gmane-link-as-kill (&optional perma)
  (interactive "P")
  (km/gnus-summary-set-current-article)
  (with-current-buffer gnus-original-article-buffer
    (--when-let (km/gnus--gmane-link perma)
      (kill-new (message it)))))

(defun km/gnus-copy-message-link (follow)
  "Copy link for current message.
If it has an \"Archived-At\" header, use that.  Otherwise, get
the link from the last widget in the buffer.  With prefix
argument FOLLOW, follow link instead of copying it."
  (interactive "P")
  (km/gnus-summary-set-current-article)
  (with-current-buffer gnus-original-article-buffer
    (save-excursion
      (--when-let (or (km/gnus--gmane-link)
                      (km/gnus--last-message-link))
        (funcall (if follow
                     #'browse-url
                   (lambda (s) (kill-new (message s))))
                 it)))))

;;;###autoload
(defun km/gnus-copy-message-id-as-kill ()
  (interactive)
  (with-current-buffer gnus-original-article-buffer
    (--when-let (message-field-value "Message-ID")
      (kill-new (message "%s" it)))))

(defun km/gnus-open-github-patch ()
  "Open patch from GitHub email."
  (interactive)
  (km/gnus-summary-set-current-article)
  (km/open-github-patch gnus-original-article-buffer))

(defun km/gnus-summary-catchup (&optional no-next)
  "Mark all articles as read.
Don't ask for confirmation.  With prefix argument NO-NEXT, exit
to group buffer instead of moving to next group."
  (interactive "P")
  (let ((gnus-auto-select-next (unless no-next 'quietly)))
    (gnus-summary-catchup-and-exit nil t)))

(defun km/shr-browse-url-and-goto-next ()
  "Run `shr-browse-url' followed by `shr-next-link'."
  (interactive)
  (shr-browse-url)
  (shr-next-link))

(defun km/gnus-pipe-to-project ()
  "Call `gnus-summary-pipe-output' in project root."
  (interactive)
  (let ((gnus-summary-pipe-output-default-command
         (format "cd %s && %s"
                 (completing-read "Project: "
                                  (projectile-relevant-known-projects))
                 (cond
                  ((not gnus-summary-pipe-output-default-command)
                   "git am")
                  ((string-match "\\`cd .* && \\(.*\\)"
                                 gnus-summary-pipe-output-default-command)
                   (match-string-no-properties
                    1 gnus-summary-pipe-output-default-command))
                  (t
                   gnus-summary-pipe-output-default-command)))))
    (call-interactively #'gnus-summary-pipe-output)))

(provide 'km-gnus)
;;; km-gnus.el ends here
