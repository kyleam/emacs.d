;;; km-mail.el --- Mail-related extensions

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
(require 'notmuch)

(require 'km-util)

;;;###autoload
(defun km/notmuch-show-open-github-patch ()
  "Open patch from GitHub email."
  (interactive)
  (with-current-notmuch-show-message
   (km/open-github-patch (current-buffer))))

;;;###autoload
(defun km/notmuch-show-pipe-message-to-project (project)
  (interactive
   (list (expand-file-name
          (completing-read "Project: "
                           (projectile-relevant-known-projects)))))
  (let ((default-directory project))
    (call-interactively #'notmuch-show-pipe-message)))

;;;###autoload
(defun km/notmuch-show-pipe-part-to-project (project)
  (interactive
   (list (expand-file-name
          (completing-read "Project: "
                           (projectile-relevant-known-projects)))))
  (let ((default-directory project))
    (call-interactively #'notmuch-show-pipe-part)))

;;;###autoload
(defun km/notmuch-archive-all ()
  "Call `notmuch-search-archive-thread' with whole-buffer region."
  (interactive)
  (mark-whole-buffer)
  (call-interactively #'notmuch-search-archive-thread))

;;;###autoload
(defun km/notmuch-tree-from-show-current-query (&optional ignore-context)
  (interactive "P")
  (let ((notmuch-show-query-context (and (not ignore-context)
                                         notmuch-show-query-context)))
    (call-interactively #'notmuch-tree-from-show-current-query)))

;;;###autoload
(defun km/notmuch-show-at-point ()
  "Call `notmuch-show' with message or thread ID at point."
  (interactive)
  (let ((id (save-excursion
              (skip-syntax-backward "^\\s-")
              (and (looking-at
                    (rx (group (zero-or-one (or "id:" "thread:")))
                        (one-or-more (any "-" "_" "." "@" "/" alnum))))
                   (concat (and (string= (match-string 1) "") "id:")
                           (match-string-no-properties 0))))))
    (if id
        (notmuch-show id)
      (call-interactively #'notmuch-show))))

(defun km/notmuch-github-pr-number ()
  "Return the PR number for this message."
  (let (pr)
    (with-current-notmuch-show-message
      (goto-char (point-min))
      (if (re-search-forward "https://github\\.com/.*/pull/\\([0-9]+\\)" nil t)
          (setq pr (match-string-no-properties 1))
        (user-error "Could not find PR number")))
    pr))

(defvar km/notmuch-github-repo-function nil
  "Function that returns repo information from this message.

If the function can determine the repository, it should return a
list, structured as (DIRECTORY REMOTE BASE).

  DIRECTORY  absolute path to the top-level of the local repo
  REMOTE     name of the remote to fetch from
  BASE       used to limit the log (i.e., \"BASE..PR-REF\").")

;;;###autoload
(defun km/notmuch-show-pr-in-magit (&optional force-fetch)
  "Show the Magit log for this message's PR.

With a prefix argument, fetch from the remote even if the ref
already exists locally.  The repository information is extracted
with `km/notmuch-github-repo-function'.

This function assumes that the remote is a GitHub repo and that
you've configured \"git fetch <remote>\" to fetch pull request
refs.  This can be done by placing a line like

        fetch = +refs/pull/*/head:refs/pull/<remote>/*

in the remote's \".git/config\" entry."
  (interactive "P")
  (require 'magit)
  (unless (functionp km/notmuch-github-repo-function)
    (user-error "`km/notmuch-github-repo-function' is not specified"))
  (let* ((info (or (funcall km/notmuch-github-repo-function)
                   (user-error "Could not determine repository")))
         (remote (or (nth 1 info) "origin"))
         (base-ref (or (nth 2 info)
                       (concat remote "/master")))
         (local-ref (format "refs/pull/%s/%s"
                            remote
                            (km/notmuch-github-pr-number)))
         (default-directory (nth 0 info)))
    (when (or force-fetch
              (not (magit-ref-exists-p local-ref)))
      (magit-call-git "fetch" remote))
    (magit-log (list (concat base-ref ".." local-ref)))))

(defmacro km/notmuch-with-raw-message (msg-id &rest body)
  "Evaluate BODY with temporary buffer containing text for MSG-ID.
MSG-ID is evaluated before entering the temporary buffer.  See
also `with-current-notmuch-show-message'."
  (declare (indent 1) (debug t))
  (let ((id (make-symbol "id")))
    `(let ((,id ,msg-id))
       (with-temp-buffer
         (let ((coding-system-for-read 'no-conversion))
           (call-process notmuch-command nil t nil "show" "--format=raw" ,id)
           (goto-char (point-min))
           ,@body)))))

(defun km/notmuch-show-debbugs-ack-info ()
  (km/notmuch-with-raw-message (notmuch-show-get-message-id)
    (when (save-excursion (re-search-forward "^X-Gnu-PR-Message: ack" nil t))
      (list
       (and (re-search-forward "^References: <\\([^>\n]+\\)>" nil t)
            (match-string 1))
       (and (re-search-forward "^Reply-To: \\([0-9]+@debbugs\\.gnu\\.org\\)"
                               nil t)
            (match-string 1))))))

;;;###autoload
(defun km/notmuch-show-stash-git-send-email-debbugs ()
  "Debbugs-aware variant of `notmuch-show-stash-git-send-email'.
If the current message is an acknowledgement from the GNU bug
Tracking System, set '--in-reply-to' to the initial report and
'--to' to the newly assigned address.  Otherwise, call
`notmuch-show-stash-git-send-email'."
  (interactive)
  (pcase-let ((`(,root-id ,bug-address) (km/notmuch-show-debbugs-ack-info)))
    (if (not (and root-id bug-address))
        (call-interactively #'notmuch-show-stash-git-send-email)
      (notmuch-common-do-stash
       (string-join
        (list (notmuch-show-stash-git-helper (list bug-address) "--to=")
              (notmuch-show-stash-git-helper
               (message-tokenize-header
                (km/notmuch-with-raw-message (concat "id:" root-id)
                  (and (re-search-forward "^Cc: \\(.+\\)" nil t)
                       (match-string 1))))
               "--cc=")
              (notmuch-show-stash-git-helper (list root-id) "--in-reply-to="))
        " ")))))


;;; Mail sync

(defvar mail-sync-log-file "/var/log/mail-sync/mail-sync")

(defun mail-sync-log-to-file (buf _)
  (with-temp-buffer
    (insert "\n")
    (insert (with-current-buffer buf (buffer-string)))
    (write-region nil nil mail-sync-log-file 'append 'no-msg)))

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
       '(mail-sync-log-to-file mail-sync-refresh-caller)))

;;;###autoload
(defun km/notmuch-sync-mail (&optional cmd-append)
  (interactive (list (if (fboundp 'km/read-sync-mail-args)
                         (km/read-sync-mail-args)
                       (read-string "sync-mail args: "))))
  (setq mail-sync-calling-buffer (current-buffer))
  (let ((default-directory (expand-file-name "~/"))
        (display-buffer-overriding-action
         '(display-buffer-below-selected)))
    (compilation-start (concat "sync-mail"
                               (and cmd-append " ")
                               cmd-append)
                       'mail-sync-mode)))
(provide 'km-mail)
;;; km-mail.el ends here
