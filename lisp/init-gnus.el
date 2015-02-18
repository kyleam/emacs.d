(require 'gnus)
(require 'org-gnus)

(setq gnus-home-directory "~/.gnus.d/"
      gnus-directory gnus-home-directory
      gnus-article-save-directory (expand-file-name "saved/" gnus-directory)
      gnus-kill-files-directory (expand-file-name "scores/" gnus-directory))

(setq gnus-startup-file (expand-file-name "newsrc" gnus-home-directory)
      gnus-init-file (expand-file-name "gnus" gnus-home-directory)
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

(setq sendmail-program "/usr/bin/msmtp"
      gnus-gcc-mark-as-read t
      gnus-visible-headers '("^From" "^Subject" "^Date" "^To" "^Cc" "^User-Agent")
      gnus-confirm-mail-reply-to-news t)

(setq imap-shell-program "/usr/lib/dovecot/imap -c ~/.dovecotrc"
      gnus-select-method '(nnimap "dov" (nnimap-stream shell))
      gnus-secondary-select-methods '((nntp "news.gmane.org")))

(setq gnus-agent-go-online t
      gnus-agent-synchronize-flags t)

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq gnus-interactive-exit nil)

(add-hook 'kill-emacs-hook 'gnus-grace-exit-before-kill-emacs)

(defun km/sync-mail ()
  (interactive)
  (let ((bufname (get-buffer-create "*Mail sync*"))
        (default-directory "~/")
        (process "mail-sync"))
    (with-current-buffer bufname
      (view-mode 1)
      (goto-char (point-max)))
    (display-buffer bufname)
    (if (process-live-p process)
        (message "Mail sync process is already running")
      (start-process process bufname km/sync-mail-cmd))))

(defvar km/sync-mail-cmd "sync-mail"
  "Run sync mail script.")

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
(define-key km/mail-map "s" 'km/sync-mail)


;;; Gnus group buffer

(setq gnus-topic-display-empty-topics nil
      gnus-group-list-inactive-groups nil)

(setq gnus-group-use-permanent-levels t)

(setq gnus-group-sort-function '(km/gnus-group-sort-by-topic
                                 gnus-group-sort-by-level))

(defun km/gnus-group-sort-by-topic (info1 info2)
  "Sort alphabetically by group topic.
This allows groups to be ordered by topics even when topic mode
is off."
  (string< (gnus-group-topic (gnus-info-group info1))
           (gnus-group-topic (gnus-info-group info2))))

(autoload 'gnus-group-topic "gnus-topic")
(defun km/gnus-group-jump-to-group ()
  "`gnus-group-jump-to-group', but with ido completion."
  (interactive)
  (gnus-group-jump-to-group
   (ido-completing-read "Group: "
                        (mapcar #'car (cdr gnus-newsrc-alist))
                        nil t)))

(defun km/gnus-topic-jump-to-topic ()
  "`gnus-group-jump-to-group', but with ido completion."
  (interactive)
  (gnus-topic-jump-to-topic
   (ido-completing-read "Topic: "
                        (mapcar #'car gnus-topic-alist)
                        nil t)))

(define-key gnus-group-mode-map [remap gnus-group-jump-to-group]
  'km/gnus-group-jump-to-group)
(define-key gnus-group-mode-map [remap gnus-topic-jump-to-topic]
  'km/gnus-topic-jump-to-topic)

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

(add-hook 'gnus-summary-mode-hook 'km/gnus-setup-local-ace-jump)

(defun km/gnus-setup-local-ace-jump ()
  (add-hook 'ace-jump-mode-end-hook (lambda () (gnus-summary-scroll-up 0))
            nil t))

(defun km/gnus-follow-last-message-link (copy)
  "Follow shr link at bottom of message.
With prefix argument COPY, just copy the link."
  (interactive "P")
  (km/gnus-summary-set-current-article)
  (with-current-buffer gnus-article-buffer
    (save-excursion
      (goto-char (point-max))
      (shr-previous-link)
      (if copy
          (shr-copy-url)
        ;; Cannot use `shr-browse-url' directly because the
        ;; `mouse-set-point' call moves point.
        (browse-url
         (get-text-property (point) 'shr-url))))))

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

(defun km/gnus-summary-set-current-article ()
  (unless gnus-summary-buffer
    (user-error "No summary buffer"))
  (with-current-buffer gnus-summary-buffer
    (save-window-excursion (gnus-summary-select-article))))

(defun km/gnus-summary-catchup (&optional no-next)
  "Mark all articles as read.
Don't ask for confirmation.  With prefix argument NO-NEXT, exit
to group buffer instead of moving to next group."
  (interactive "P")
  (let ((gnus-auto-select-next (unless no-next 'quietly)))
    (gnus-summary-catchup-and-exit nil t)))

(defun km/gnus-copy-gmane-link-as-kill ()
  (interactive)
  (km/gnus-summary-set-current-article)
  (with-current-buffer gnus-original-article-buffer
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "Archived-At: <\\(.*\\)>")
          (let ((link (match-string-no-properties 1)))
            (kill-new (message "%s" link)))
        (user-error "No link found")))))

(define-key gnus-summary-mode-map
  (kbd "C-c j") 'km/gnus-follow-last-message-link)
(define-key gnus-summary-mode-map ";" 'gnus-summary-universal-argument)
;; This overrides `gnus-summary-post-news', which is also bound to
;; 'S p'.
(define-key gnus-summary-mode-map "c" 'km/gnus-summary-catchup)
(define-key gnus-summary-mode-map "e" 'gnus-summary-scroll-up)
(define-key gnus-summary-mode-map "j" 'ace-jump-mode)

(define-key gnus-article-mode-map
  (kbd "C-c j") 'km/gnus-follow-last-message-link)
(define-key gnus-article-mode-map "e" 'shr-browse-url)

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
      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "%f wrote:"
      message-kill-buffer-on-exit t
      footnote-section-tag "")

(add-hook 'message-send-hook 'km/message-confirm-sender)
(add-hook 'message-mode-hook 'flyspell-mode)

(defun km/message-confirm-sender ()
  "Stop sending message from the wrong address."
  (unless (yes-or-no-p (format "Send message from %s?"
                               (message-field-value "From")))
    (user-error "Not sending message")))

;; Modified from
;; http://emacs-fu.blogspot.com/2008/12/some-simple-tricks-boxquote-footnote.html.
(defun km/snip-mail-quote (beg end &optional number quote-char)
  "Replace region lines with \"[...]\".
If prefix argument NUMBER is non-nil, add the number of lines
that were snipped.  The default QUOTE-CHAR is \">\".  Place text
following the snipped lines on a new line, and file the resulting
paragraph."
  (interactive "r\nP")
  (let ((nlines (count-lines beg end))
        (quote-char (or quote-char ">")))
    (delete-region beg end)
    (if number
        (insert (format "[%d line%s ...]" nlines (if (= 1 nlines) "" "s")))
      (insert (format "[...]")))
    (search-backward "[")
    (unless (bolp)
      (newline))
    (search-forward "]")
    (unless (eolp)
      (newline)
      (insert quote-char)
      (just-one-space)
      (fill-paragraph))))

(define-key message-mode-map (kbd "C-c m s") 'km/snip-mail-quote)


;;; Select and bury

;; Modified from http://www.xsteve.at/prg/gnus/

(defun km/gnus-select-or-bury (&optional plugged)
  "Start, select, or bury gnus.
Prefix argument PLUGGED is passed to `gnus-unbury'."
  (interactive "P")
  (if (km/gnus-bufferp (current-buffer))
      (km/gnus-bury)
    (km/gnus-unbury plugged)))

(defvar km/gnus-window-configuration nil)

(defun km/gnus-unbury (&optional plugged)
  "Unbury Gnus-related buffers.
If PLUGGED is non-nil, start Gnus in a plugged state.  This only
has an effect if Gnus is not currently open."
  (cond
   (km/gnus-window-configuration
    (set-window-configuration km/gnus-window-configuration))
   ((get-buffer "*Group*")
    (delete-other-windows)
    (pop-to-buffer-same-window "*Group*"))
   (t
    (setq gnus-plugged plugged)
    (gnus)))
  (setq km/gnus-window-configuration nil))

(defun km/gnus-bury ()
  (when (km/gnus-bufferp (current-buffer))
    (setq km/gnus-window-configuration (current-window-configuration))
    (--each (km/gnus-buffer-list)
      (if (eq (current-buffer) it)
          (progn
            (delete-other-windows)
            (bury-buffer))
        (bury-buffer it)))))

(defun km/gnus-bufferp (buffer)
  (with-current-buffer buffer
    (derived-mode-p 'gnus-group-mode
                    'gnus-summary-mode
                    'gnus-article-mode
                    'message-mode)))

(defun km/gnus-buffer-list ()
  (-filter #'km/gnus-bufferp (buffer-list)))

(define-key km/mail-map "b" 'km/gnus-select-or-bury)


;;; Notmuch

(require 'notmuch)
(require 'org-notmuch)

(setq org-gnus-prefer-web-links t)

(setq notmuch-fcc-dirs nil
      notmuch-search-oldest-first nil)

(add-hook 'km/org-store-link-hook 'km/gnus-goto-message-in-notmuch)

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

(defun km/gnus-goto-message-in-notmuch ()
  "Show message in notmuch."
  (interactive)
  (when (and (memq major-mode '(gnus-summary-mode gnus-article-mode))
             (string= (cadr (gnus-find-method-for-group gnus-newsgroup-name))
                      "dov"))
    (let* ((header (with-current-buffer gnus-summary-buffer
                     (gnus-summary-article-header)))
           (message-id (org-remove-angle-brackets (mail-header-id header))))
      (notmuch-show (concat "id:" message-id)))))

(define-key notmuch-show-mode-map (kbd "C-c C-c") 'km/notmuch-goto-message-in-gnus)
(define-key gnus-group-mode-map "GG" 'notmuch-search)

(define-key km/mail-map "n" 'notmuch-search)

(provide 'init-gnus)
