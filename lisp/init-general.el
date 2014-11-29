(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t
      shell-command-switch "-ic"
      x-select-enable-clipboard t ; Share clipboard with system.
      x-select-enable-primary t
      ispell-program-name "aspell"
      whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark)
      tramp-default-method "ssh"
      sentence-end-double-space nil
      enable-recursive-minibuffers t
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setq-default indicate-empty-lines t)

(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Set location of custom.el.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(global-auto-revert-mode t)

;; Make scripts executable at save.
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(transient-mark-mode -1)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(key-chord-mode 1)

(global-set-key (kbd "C-h :") 'find-function)

;; This is also bound to 'm', but I always want to press 'j' because
;; binding for `imenu' and `org-goto'.
(define-key Info-mode-map "j" 'Info-menu)

;; Disable `suspend-frame' binding.
(global-unset-key (kbd "C-x C-z"))

;; Avoid shift key for `backward-paragraph' and `forward-paragraph'.
(global-unset-key (kbd "M-}"))
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-unset-key (kbd "M-{"))
(global-set-key (kbd "M-[") 'backward-paragraph)


(defun km/imenu (rescan)
  "Call `imenu', rescanning if RESCAN is non-nil"
  (interactive "P")
  (when rescan
    ;; Taken from `imenu-choose-buffer-index'.
    (imenu--cleanup)
    (setq imenu--index-alist nil))
  (call-interactively #'imenu))

(global-set-key (kbd "C-c j") 'km/imenu)

;; Taken from
;; http://milkbox.net/note/single-file-master-emacs-configuration/.
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(define-key occur-mode-map "n" 'next-line)
(define-key occur-mode-map "p" 'previous-line)

(provide 'init-general)
