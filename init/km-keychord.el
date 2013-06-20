(require 'key-chord)
(key-chord-mode 1)

(key-chord-define-global ",r" 'km/recentf-ido-find-file)
(key-chord-define-global ",t" 'org-capture)
;; instead of alt-x
(key-chord-define-global ",x" 'smex)
(key-chord-define-global ",f" 'find-file)
(key-chord-define-global ",g" 'magit-status)
(key-chord-define-global ",b" 'ido-switch-buffer)

(key-chord-define-global ",s" 'save-buffer)
(key-chord-define-global ",q" 'kill-this-buffer)
(key-chord-define-global ",d" 'km/save-and-kill-buffer)
(key-chord-define-global ",e" 'eval-region)

(key-chord-define-global ",w" 'km/sync-mail)

(key-chord-define-global ",c" 'km/toggle-line-or-region-comment)
