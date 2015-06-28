(require 'god-mode)

(defvar km/god-exempt-major-modes
  '(gnus-group-mode
    gnus-summary-mode
    gnus-article-mode
    message-mode
    magit-popup-mode
    magit-popup-sequence-mode))

(setq god-exempt-major-modes
      (append km/god-exempt-major-modes god-exempt-major-modes))

;; `god-exempt-major-modes' can't be used here because buffer is in
;; `text mode'.
(add-hook 'git-commit-setup-hook (lambda () (god-local-mode -1)))

(add-hook 'view-mode-hook (lambda ()
                            (if view-mode
                                (god-local-mode-pause)
                              (god-local-mode-resume))))

(add-hook 'org-capture-mode-hook (lambda () (god-local-mode -1)))

(add-hook 'god-mode-enabled-hook (lambda ()
                                   (when view-mode
                                     (view-mode -1))
                                   (when (derived-mode-p 'emacs-lisp-mode)
                                     (lispy-mode -1))))

(add-hook 'god-mode-disabled-hook (lambda ()
                                    (when (derived-mode-p 'emacs-lisp-mode)
                                      (lispy-mode 1))))

(add-hook 'god-mode-enabled-hook 'km/god-update-cursor)
(add-hook 'god-mode-disabled-hook 'km/god-update-cursor)

(defun km/god-update-cursor ()
  (setq cursor-type (if god-local-mode 'bar 'box)))

(global-set-key (kbd "C-c d") 'god-local-mode)

(define-key god-local-mode-map "." 'repeat)
(define-key god-local-mode-map "i" 'god-local-mode)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(provide 'init-god)
