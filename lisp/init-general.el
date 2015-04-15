
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t
      enable-recursive-minibuffers t)

(setq tramp-default-method "ssh")

(setq-default indicate-empty-lines t
              indent-tabs-mode nil)

(setq set-mark-command-repeat-pop t)

;; This is intentionally not loaded.
(setq custom-file "~/.emacs.d/.custom.el")

(setq bookmark-save-flag nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defun km/imenu (rescan)
  "Call `imenu', rescanning if RESCAN is non-nil."
  (interactive "P")
  (when (and rescan
             ;; No need to rescan if imenu hasn't been autoloaded yet.
             (fboundp 'imenu--cleanup))
    ;; Taken from `imenu-choose-buffer-index'.
    (imenu--cleanup)
    (setq imenu--index-alist nil))
  (call-interactively #'helm-imenu))

;; Taken from
;; http://milkbox.net/note/single-file-master-emacs-configuration/.
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(global-set-key (kbd "C-h ;") 'find-function)
(global-set-key (kbd "C-h 4 ;") 'find-function-other-window)


(global-set-key (kbd "C-c l") 'km/imenu)

;; Disable `suspend-frame' binding.
(global-set-key (kbd "C-x C-z") nil)

;; Avoid shift key for `backward-paragraph' and `forward-paragraph'.
(global-set-key (kbd "M-}") nil)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-{") nil)
(global-set-key (kbd "M-[") 'backward-paragraph)

(global-set-key (kbd "C-c x") 'eval-expression)

(define-key occur-mode-map "n" 'next-line)
(define-key occur-mode-map "p" 'previous-line)

(show-paren-mode)
(global-auto-revert-mode)
(key-chord-mode 1)

(provide 'init-general)
