(require 'gnus)

(setq
 ;; Locations
 gnus-home-directory "~/.gnus.d"
 gnus-directory gnus-home-directory
 gnus-article-save-directory (expand-file-name "saved" gnus-directory)
 gnus-kill-files-directory (expand-file-name "scores" gnus-directory)
 ;; Startup files
 gnus-startup-file (expand-file-name "newsrc" gnus-home-directory)
 gnus-init-file (expand-file-name "gnus" gnus-home-directory)
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil
 ;; Select methods
 imap-shell-program "/usr/lib/dovecot/imap -c ~/.dovecotrc"
 gnus-select-method '(nnimap "dov" (nnimap-stream shell))
 gnus-secondary-select-methods '((nntp "news.gmane.org"))
 ;; Groups
 gnus-topic-display-empty-topics nil
 gnus-group-list-inactive-groups nil
 ;; Messages
 message-send-mail-function 'message-send-mail-with-sendmail
 sendmail-program "/usr/bin/msmtp"
 message-sendmail-envelope-from 'header
 gnus-gcc-mark-as-read t
 message-citation-line-function 'message-insert-formatted-citation-line
 message-citation-line-format "On %D %R, %N wrote:"
 message-kill-buffer-on-exit t
 gnus-visible-headers '("^From" "^Subject" "^Date" "^To" "^Cc" "^User-Agent")
 gnus-confirm-mail-reply-to-news t
 footnote-section-tag ""
 ;; Threading
 gnus-thread-hide-subtree t
 gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-number)
 gnus-summary-line-format "%U%R %&user-date;%-20= %-15,15f  %B %S \n"
 gnus-sum-thread-tree-indent "  "
 gnus-sum-thread-tree-root "."
 gnus-sum-thread-tree-false-root "o "
 gnus-sum-thread-tree-single-indent ""
 gnus-sum-thread-tree-leaf-with-other "+-> "
 gnus-sum-thread-tree-vertical "| "
 gnus-sum-thread-tree-single-leaf "`-> "
 ;; Agent
 gnus-agent-go-online t
 gnus-agent-synchronize-flags t
 ;; Miscellaneous
 mm-discouraged-alternatives '("text/html" "text/richtext")
 gnus-interactive-exit nil)

(defun km/sync-mail ()
  (interactive)
  (let ((bufname (get-buffer-create "*Mail sync*"))
        (default-directory "~")
        (process "mail-sync"))
    (with-current-buffer bufname
      (view-mode 1)
      (goto-char (point-max)))
    (display-buffer bufname)
    (if (process-live-p process)
        (message "Mail sync process is already running")
      (start-process process bufname km/sync-mail-cmd))))

(defvar km/sync-mail-cmd "sync-mail.sh"
  "Run sync mail script.")

(define-key external-map "m" 'km/sync-mail)

;; http://www.emacswiki.org/emacs/GnusSync
(defun gnus-grace-exit-before-kill-emacs ()
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (let ((noninteractive t))
        (gnus-group-exit))))
(add-hook 'kill-emacs-hook 'gnus-grace-exit-before-kill-emacs)

;; Start in topic mode.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(add-hook 'message-mode-hook
          '(lambda ()
             (flyspell-mode 1)))

(defun km/follow-last-message-link ()
  "Follow link at bottom of message."
  (interactive)
  (km/gnus-end-of-article-buffer)
  (widget-backward 1)
  (widget-button-press (point)))

(define-key gnus-summary-mode-map
  (kbd "C-c j") 'km/follow-last-message-link)
(define-key gnus-article-mode-map
  (kbd "C-c j") 'km/follow-last-message-link)

(defun km/gnus-open-github-patch ()
  "Open patch from github email.
A new buffer with the patch contents is opened in another window."
  (interactive)
  (km/gnus-end-of-article-buffer)
  (search-backward "patch")
  (let ((url (thing-at-point 'url))
        (patch-buffer (generate-new-buffer "*gnus-github-patch*")))
    (switch-to-buffer-other-window patch-buffer)
    (url-insert-file-contents url)
    (diff-mode)
    (view-mode 1)))

(defun km/gnus-end-of-article-buffer ()
  "Move point to the end of the article buffer."
  ;; The next 3 lines are from `gnus-summary-widget-forward'.
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (select-window (gnus-get-buffer-window gnus-article-buffer))
  (goto-char (point-max)))

(require 'notmuch)
(require 'org-gnus)

(define-key gnus-group-mode-map "GG" 'notmuch-search)
;; http://roland.entierement.nu/blog/2010/09/08/gnus-dovecot-offlineimap-search-a-howto.html
(defun km/notmuch-shortcut ()
  (define-key gnus-group-mode-map "GG" 'notmuch-search))

(defun km/notmuch-file-to-group (file)
  "Calculate the Gnus group name from the given file name."
  (let ((group (file-name-directory (directory-file-name (file-name-directory file)))))
    (setq group (replace-regexp-in-string ".*/mail/" "nnimap+dov:" group))
    (setq group (replace-regexp-in-string "/$" "" group))
    (if (string-match ":$" group)
        (concat group "INBOX")
      (replace-regexp-in-string ":\\." ":" group))))

(defun km/notmuch-goto-message-in-gnus ()
  "Open a summary buffer containing the current notmuch article."
  (interactive)
  (let ((group (km/notmuch-file-to-group (notmuch-show-get-filename)))
        (message-id (replace-regexp-in-string
                     "^id:" "" (notmuch-show-get-message-id))))
    (setq message-id (replace-regexp-in-string "\"" "" message-id))
    (if (and group message-id)
        (progn
    (switch-to-buffer "*Group*")
    (org-gnus-follow-link group message-id))
      (message "Couldn't get relevant infos for switching to Gnus."))))

(define-key notmuch-show-mode-map (kbd "C-c C-c") 'km/notmuch-goto-message-in-gnus)
(add-hook 'gnus-group-mode-hook 'km/notmuch-shortcut)

(setq notmuch-fcc-dirs nil)

;; Modified from
;; http://emacs-fu.blogspot.com/2008/12/some-simple-tricks-boxquote-footnote.html.
(defun km/snip-mail-quote (beg end &optional no-number quote-char)
  "Replace region lines with \"[n lines ...]\".
If NO-NUMBER is non-nil (or when called interactively with a
prefix argument), the number of lines is not added. The default
QUOTE-CHAR is \">\". Text following the snipped lines is placed
on a new line and the resulting paragraph is filled."
  (interactive "r\nP")
  (let ((nlines (count-lines beg end))
        (quote-char (or quote-char ">")))
    (delete-region beg end)
    (if no-number
        (insert (format "[...]"))
      (insert (format "[%d line%s ...]" nlines (if (= 1 nlines) "" "s"))))
    (search-backward "[")
    (unless (bolp)
      (newline))
    (search-forward "]")
    (unless (eolp)
      (newline)
      (insert quote-char)
      (just-one-space)
      (fill-paragraph))))

(define-key message-mode-map
  (kbd "C-c m s") 'km/snip-mail-quote)

(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (gnus-define-keys gnus-summary-mode-map
              "c" km/gnus-catchup-and-goto-next-group
              "j" gnus-summary-next-unread-article
              ";" gnus-summary-universal-argument  ;; mutt's tag
              "e" gnus-summary-scroll-up)))

(add-hook 'gnus-group-mode-hook
          (lambda ()
            (gnus-define-keys gnus-group-mode-map
              "e" gnus-topic-select-group)))

(add-hook 'gnus-article-mode-hook
          (lambda ()
            (gnus-define-keys gnus-article-mode-map
              "e" shr-browse-url)))

;; Modified from http://www.xsteve.at/prg/gnus/

(defun km/gnus ()
  "Start, select, or bury gnus."
  (interactive)
  (let ((bufname (buffer-name)))
    (if (or (string-equal "*Group*" bufname)
            (string-match "\*Summary" bufname)
            (string-match "\*Article" bufname))
        (km/bury-gnus)
      (if (get-buffer "*Group*")
          (km/unbury-gnus)
        (gnus-unplugged)))))

(defvar gnus-bury-window-configuration nil)

(defun km/unbury-gnus ()
  (let (dead-frame-p
        (windows-saved-p
         (and (boundp 'gnus-bury-window-configuration)
              gnus-bury-window-configuration)))
    (when windows-saved-p
      (unless (set-window-configuration gnus-bury-window-configuration)
        (setq dead-frame-p t)))
    (when (or dead-frame-p (not windows-saved-p))
      (switch-to-buffer "*Group*"))))

(defun km/bury-gnus ()
  (let ((buf nil)
        (bufname nil))
    (setq gnus-bury-window-configuration nil)
    (dolist (buf (buffer-list))
      (setq bufname (buffer-name buf))
      (when (or (string-equal "*Group*" bufname)
                (string-match "\*Summary" bufname)
                (string-match "\*Article" bufname))
        (unless gnus-bury-window-configuration
          (setq gnus-bury-window-configuration (current-window-configuration)))
        (delete-other-windows)
        (if (eq (current-buffer) buf)
            (bury-buffer)
          (bury-buffer buf))))))

(global-set-key (kbd "C-x m") 'km/gnus)

;; From http://ivan.kanis.fr/ivan-gnus.el
(defun km/gnus-catchup-and-goto-next-group (&optional all)
  "Mark all articles in this group as read and select the next group.
If given a prefix, mark all articles, unread as well as ticked, as
read. Don't ask to confirm."
  (interactive "P")
  (save-excursion
    (gnus-summary-catchup all t))
  (gnus-summary-next-group))

;; From http://ivan.kanis.fr/ivan-gnus.el
(defadvice gnus-summary-next-group (before km/gnus-next-group activate)
  "Go to next group without selecting the first article."
  (ad-set-arg 0 t))

(provide 'init-gnus)
