
(setq server-use-tcp t)
(require 'server)
(unless (server-running-p)
  (server-start))

(let ((server (daemonp)))
  (cond
   ((string= server "default")
    (global-set-key (kbd "C-x m") nil)
    (setq save-abbrevs 'silently
          bookmark-save-flag 1))
   ((string= server "mail")
    (setq mode-line-misc-info (cons " [Mail] " mode-line-misc-info))
    (key-chord-define-global "jg" 'km/mail-map)
    (setq recentf-save-file "~/.emacs.d/cache/recentf-mail")
    (setq save-abbrevs nil))))

(provide 'init-server)
