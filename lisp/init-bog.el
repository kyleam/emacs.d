(require 'bog)

(setq bog-completing-read 'ido-completing-read
      bog-read-file-name 'ido-read-file-name)

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c b") bog-mode-map))

(provide 'init-bog)
