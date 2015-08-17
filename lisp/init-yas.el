
(require 'yasnippet)

(setq yas-fallback-behavior nil)

(defvar km/personal-snippets
  (file-name-as-directory (expand-file-name "psnippets" user-emacs-directory)))

(when (file-exists-p km/personal-snippets)
  (add-to-list 'yas-snippet-dirs km/personal-snippets))

(defun km/yas-with-comment (str)
  (concat comment-start
          (unless (s-ends-with? " " comment-start) " ")
          str comment-end))

(define-key yas-minor-mode-map (kbd "C-c i") 'yas-expand)
;; Remove commands with 'C-c &' prefix, which conflicts with
;; `org-mark-ring-goto' binding'
(define-key yas-minor-mode-map (kbd "C-c &") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(yas-global-mode)

(provide 'init-yas)
