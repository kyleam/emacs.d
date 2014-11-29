
(smex-initialize)

(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(key-chord-define-global ",x" 'smex)

(provide 'init-smex)
