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

(require 'dash)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-topic)
(require 'gnus-sum)
(require 'notmuch)
(require 'shr)

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

(defun km/gnus-open-github-patch ()
  "Open patch from github email.
A new buffer with the patch contents is opened in another window."
  (interactive)
  (km/gnus-summary-set-current-article)
  (let ((bufname (generate-new-buffer-name "*gnus-github-patch*"))
        url)
    (with-current-buffer gnus-original-article-buffer
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "https://github.com/.*\\.patch")
            (setq url (match-string-no-properties 0))
          (user-error "No patch found"))))
    (with-current-buffer (get-buffer-create bufname)
      (url-insert-file-contents url)
      (diff-mode)
      (view-mode 1))
    (pop-to-buffer bufname)))

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


;;; Message mode

(defun km/message-confirm-sender ()
  "Stop sending message from the wrong address."
  (unless (yes-or-no-p (format "Send message from %s?"
                               (message-field-value "From")))
    (user-error "Not sending message")))


;;; Notmuch

(require 'notmuch)

;;;###autoload
(defun km/notmuch-show-copy-message-id-as-kill ()
  (interactive)
  (kill-new (message "%s" (notmuch-show-get-message-id))))

(provide 'km-mail)
;;; km-mail.el ends here
