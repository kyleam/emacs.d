(require 'bog)

(eval-after-load 'org
  '(progn
     (define-prefix-command 'bog-map)
     (define-key org-mode-map (kbd "C-c b") 'bog-map)
     (define-key bog-map "p" 'bog-find-citekey-pdf)
     (define-key bog-map "r" 'bog-rename-staged-pdf-to-citekey)
     (define-key bog-map "b" 'bog-find-citekey-bib)
     (define-key bog-map "w" 'bog-search-citekey-on-web)))

(provide 'init-bog)
