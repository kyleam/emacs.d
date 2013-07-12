;; set up gnus directories before gnus is initialized
(setq gnus-directory "~/.gnus.d"
      gnus-home-directory "~/.gnus.d"
      message-directory "~/.gnus.d/mail"
      message-auto-save-directory "~/.gnus.d/drafts"
      nnml-directory "~/.gnus.d/nnml-mail"
      nnfolder-directory "~/.gnus.d/mail/archive"
      nnfolder-active-file "~/.gnus.d/mail/archive/active"
      gnus-article-save-directory "~/.gnus.d/saved"
      gnus-kill-files-directory "~/.gnus.d/scores"
      gnus-cache-directory "~/.gnus.d/cache")

(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

(defun km/sync-mail ()
  (interactive)
  (let ((bufname (get-buffer-create "*Mail sync*")))
    (start-process "mail sync" bufname km/sync-mail-cmd)))

(defvar km/sync-mail-cmd "~/bin/sync-mail.sh"
  "Run sync mail script")

;; http://www.emacswiki.org/emacs/GnusSync
(defun gnus-grace-exit-before-kill-emacs ()
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (let ((noninteractive t))
        (gnus-group-exit))))
(add-hook 'kill-emacs-hook 'gnus-grace-exit-before-kill-emacs)

(setq imap-shell-program "/usr/lib/dovecot/imap -c ~/.dovecotrc"
      gnus-select-method '(nnimap "dov" (nnimap-stream shell))
      gnus-secondary-select-methods '((nntp "news.gmane.org")))

(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp")

(setq gnus-gcc-mark-as-read t)

(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %D %R, %N wrote:")

(setq message-kill-buffer-on-exit t)

;; for rss too specific to add to gwene
(require 'nnrss)

(setq gnus-group-list-inactive-groups nil)

;; start in topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-topic-display-empty-topics nil)

(setq gnus-visible-headers
      '("^From" "^Subject" "^Date" "^To" "^Cc" "^User-Agent"))

(add-hook 'message-mode-hook
          '(lambda ()
             (flyspell-mode 1)
             (orgstruct-mode 1)))

(setq gnus-confirm-mail-reply-to-news t)

(gnus-define-keys gnus-summary-mode-map
  "j" gnus-summary-next-unread-article
  ";" gnus-summary-universal-argument  ;; mutt's tag
  )

(global-set-key (kbd "C-x m") 'gnus-group-mail)

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-summary-line-format
      "%U%R %&user-date;%-20= %-15,15f  %B %S \n")
(setq gnus-sum-thread-tree-indent "  "
      gnus-sum-thread-tree-root "."
      gnus-sum-thread-tree-false-root "o "
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-vertical "| "
      gnus-sum-thread-tree-single-leaf "`-> ")

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-most-recent-date))

(defun km/follow-gwene-link ()
  "Follow link at bottom of gwene message"
  (interactive)
  ;; next 3 lines from gnus-sum.el guns-summary-widget-forward
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (select-window (gnus-get-buffer-window gnus-article-buffer))

  (end-of-buffer)
  (search-backward "Link")
  (widget-button-press (point)))

(define-key gnus-summary-mode-map
  (kbd "C-c j") 'km/follow-gwene-link)
(define-key gnus-article-mode-map
  (kbd "C-c j") 'km/follow-gwene-link)

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

(setq footnote-section-tag "")
