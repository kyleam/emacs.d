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

(setq gnus-agent-go-online t
      gnus-agent-synchronize-flags t)

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
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

(setq gnus-group-sort-function '(km/gnus-group-sort-by-topic
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
  (call-interactively #'avy-goto-subword-1)
  (gnus-summary-scroll-up 0))

(defun km/gnus-follow-last-message-link (copy)
  "Follow shr link at bottom of message.
With prefix argument COPY, just copy the link."
  (interactive "P")
  (km/gnus-summary-set-current-article)
  (with-current-buffer gnus-article-buffer
    (save-excursion
      (goto-char (point-max))
      (widget-forward -1)
      (if copy
          (kill-new (or (get-text-property (point) 'gnus-string)
                        (get-text-property (point) 'shr-url)))
        (widget-button-press (point))))))

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

(defun km/shr-browse-url-and-goto-next ()
  "Run `shr-browse-url' followed by `shr-next-link'."
  (interactive)
  (shr-browse-url)
  (shr-next-link))

;; This overrides `gnus-summary-goto-last-article', which is also
;; bound to 'G l'.
(define-key gnus-summary-mode-map "l" 'km/gnus-follow-last-message-link)
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
  (define-key shr-map "O" 'shr-save-contents))

(define-key gnus-article-mode-map
  (kbd "C-c l") 'km/gnus-follow-last-message-link)
(define-key gnus-article-mode-map "e" 'km/shr-browse-url-and-goto-next)
;; This overrides `gnus-summary-save-article', which is also on 'O o'.
(define-key gnus-article-mode-map "o" 'km/ace-link-widget)

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


;;; Notmuch

(require 'notmuch)
(require 'org-notmuch)

(setq org-gnus-prefer-web-links t)

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
