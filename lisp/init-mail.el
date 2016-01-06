;;; init-mail.el --- Mail configuration

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

(require 'gnus)
(require 'org-gnus)

(setq gnus-home-directory "~/.gnus.d/"
      gnus-directory gnus-home-directory
      gnus-article-save-directory (expand-file-name "saved/" gnus-directory)
      gnus-kill-files-directory (expand-file-name "scores/" gnus-directory))

(setq gnus-startup-file (expand-file-name "newsrc" gnus-home-directory)
      gnus-init-file (expand-file-name "gnus" gnus-home-directory)
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-inhibit-startup-message t)

(setq sendmail-program "/usr/bin/msmtp"
      gnus-gcc-mark-as-read t
      gnus-visible-headers '("^From" "^Subject" "^Date" "^To" "^Cc" "^User-Agent")
      gnus-confirm-mail-reply-to-news t)

(setq gnus-agent-go-online t
      gnus-agent-synchronize-flags t)

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq gnus-inhibit-images t)

(setq gnus-interactive-exit nil)

(add-hook 'kill-emacs-hook 'gnus-grace-exit-before-kill-emacs)

;; http://www.emacswiki.org/emacs/GnusSync
(defun gnus-grace-exit-before-kill-emacs ()
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (let ((noninteractive t))
        (gnus-group-exit))))

(define-prefix-command 'km/mail-map)
(global-set-key (kbd "C-x m") 'km/mail-map)

(define-key km/mail-map "g" 'gnus)
(define-key km/mail-map "p" 'gnus-plugged)
(define-key km/mail-map "u" 'gnus-unplugged)


;;; Gnus group buffer

(setq gnus-topic-display-empty-topics nil
      gnus-group-line-format "%M\%S\%p\%P\%5y:%B%(%G%)\n"
      gnus-group-list-inactive-groups nil)

(setq gnus-group-use-permanent-levels t)

(setq gnus-group-sort-function '(gnus-group-sort-by-alphabet
                                 km/gnus-group-sort-by-topic
                                 gnus-group-sort-by-level))

(defun km/gnus-group-sort-by-topic (info1 info2)
  "Sort alphabetically by group topic.
This allows groups to be ordered by topics even when topic mode
is off."
  (require 'gnus-topic)
  (string< (gnus-group-topic (gnus-info-group info1))
           (gnus-group-topic (gnus-info-group info2))))

(define-key gnus-group-mode-map "e" 'gnus-group-select-group)


;;; Gnus summary and article buffer

(setq gnus-summary-line-format "%U%R %&user-date;%-20= %-15,15f  %B %S \n"
      gnus-sum-thread-tree-indent "  "
      gnus-sum-thread-tree-root "."
      gnus-sum-thread-tree-false-root "o "
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-vertical "| "
      gnus-sum-thread-tree-single-leaf "`-> ")

(setq gnus-auto-select-next 'quietly)

(setq gnus-thread-hide-subtree t
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-number))

(defun km/gnus-avy-goto-subword-and-select ()
  (interactive)
  (let (avy-all-windows)
    (call-interactively #'avy-goto-subword-1))
  (gnus-summary-scroll-up 0))

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

;; This overrides `gnus-summary-goto-last-article', which is also
;; bound to 'G l'.
(define-key gnus-summary-mode-map "l" 'km/gnus-copy-message-link)
(define-key gnus-summary-mode-map "v" 'org-capture)
(define-key gnus-summary-mode-map ";" 'gnus-summary-universal-argument)
;; This overrides `gnus-summary-post-news', which is also bound to
;; 'S p'.
(define-key gnus-summary-mode-map "c" 'km/gnus-summary-catchup)
(define-key gnus-summary-mode-map "e" 'gnus-summary-scroll-up)
(define-key gnus-summary-mode-map "j" 'km/gnus-avy-goto-subword-and-select)
(define-key gnus-summary-mode-map "o" 'km/ace-link-widget)

;; Allow `km/ace-link-widget' binding to work even when on shr widget.
(after 'shr
  (define-key shr-map "o" nil)
  (define-key shr-map "O" 'shr-save-contents)
  (define-key shr-map "v" nil))

(define-key gnus-article-mode-map
  (kbd "C-c l") 'km/gnus-follow-last-message-link)
(define-key gnus-article-mode-map "e" 'km/shr-browse-url-and-goto-next)
;; This overrides `gnus-summary-save-article', which is also on 'O o'.
(define-key gnus-article-mode-map "o" 'km/ace-link-widget)
(define-key gnus-article-mode-map "v" 'org-capture)

(define-prefix-command 'km/gnus-summary-prefix-map)
(define-key gnus-summary-mode-map (kbd "C-c m") 'km/gnus-summary-prefix-map)
(define-key km/gnus-summary-prefix-map "p" 'km/gnus-open-github-patch)
(define-key km/gnus-summary-prefix-map "l" 'km/gnus-copy-gmane-link-as-kill)

(define-prefix-command 'km/gnus-article-prefix-map)
(define-key gnus-article-mode-map (kbd "C-c m") 'km/gnus-article-prefix-map)
(define-key km/gnus-article-prefix-map "p" 'km/gnus-open-github-patch)
(define-key km/gnus-article-prefix-map "l" 'km/gnus-copy-gmane-link-as-kill)


;;; Message mode

(setq message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-envelope-from 'header
      message-kill-buffer-on-exit t
      footnote-section-tag "")

(add-hook 'message-send-hook 'km/message-confirm-sender)
(add-hook 'message-mode-hook 'flyspell-mode)

(defun km/message-confirm-sender ()
  "Stop sending message from the wrong address."
  (unless (yes-or-no-p (format "Send message from %s?"
                               (message-field-value "From")))
    (user-error "Not sending message")))


;;; Notmuch

(require 'notmuch)
(require 'org-notmuch)

(setq notmuch-fcc-dirs nil
      notmuch-search-oldest-first nil)

(add-to-list 'notmuch-saved-searches
             '(:name "today" :query "date:today.." :key "."))

(defun km/notmuch-show-copy-message-id-as-kill ()
  (interactive)
  (kill-new (message "%s" (notmuch-show-get-message-id))))

(define-key notmuch-hello-mode-map "o" 'km/ace-link-widget)

(define-key km/mail-map "n" 'notmuch)

(define-prefix-command 'km/notmuch-show-prefix-map)
(define-key notmuch-show-mode-map (kbd "C-c m") 'km/notmuch-show-prefix-map)

(define-key km/notmuch-show-prefix-map "i"
  'km/notmuch-show-copy-message-id-as-kill)

(provide 'init-mail)
;;; init-mail.el ends here
