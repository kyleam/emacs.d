(require 'key-chord)
(key-chord-mode 1)

(key-chord-define-global ",r" 'km/recentf-ido-find-file)
(key-chord-define-global ",t" 'org-capture)
;; Instead of alt-x
(key-chord-define-global ",x" 'smex)
(key-chord-define-global ",f" 'find-file)
(key-chord-define-global ",g" 'magit-status)
(key-chord-define-global ",b" 'ido-switch-buffer)

(key-chord-define-global ",s" 'save-buffer)
(key-chord-define-global ",q" 'kill-this-buffer)
(key-chord-define-global ",d" 'km/save-and-kill-buffer)
(key-chord-define-global ",e" '(lambda ()
                                 (interactive)
                                 (save-buffer)
                                 (server-edit)))

(key-chord-define-global ",c" 'km/toggle-line-or-region-comment)

(key-chord-define-global ";a" 'ace-jump-mode)
(key-chord-define-global ",z" 'rgrep)
(key-chord-define-global ",v" 'view-mode)

(key-chord-define-global ",a" 'org-agenda)

;; Projectile key chords
(key-chord-define-global ";s" 'projectile-switch-project)
(key-chord-define-global ";f" 'projectile-find-file)
(key-chord-define-global ";d" 'projectile-find-dir)
(key-chord-define-global ";g" 'projectile-grep)
(key-chord-define-global ";r" 'projectile-replace)
(key-chord-define-global ";c" 'projectile-commander)
