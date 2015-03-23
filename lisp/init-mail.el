
(require 'init-gnus)

(if (equal (daemonp) "mail")
    (progn
      (setq mode-line-misc-info (cons " [Mail] " mode-line-misc-info))
      (key-chord-define-global "jg" 'km/mail-map))
  (global-set-key (kbd "C-x m") nil))

(provide 'init-mail)
