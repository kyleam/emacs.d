
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(key-chord-define-global ",x" 'smex)

(smex-initialize)

(provide 'init-smex)
