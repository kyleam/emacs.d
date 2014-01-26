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
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(setq-default indicate-empty-lines t)

(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Set location of custom.el.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Store all backup and autosave files in tmp dir.
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

;; Make scripts executable at save.
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(transient-mark-mode -1)

(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "M-/") 'hippie-expand)
;; http://www.emacswiki.org/emacs/HippieExpand#toc9
(defadvice he-substitute-string (after he-paredit-fix activate)
  "Remove extra paren when expanding line in paredit."
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

(eval-after-load 'view
  '(progn
     (define-key view-mode-map "l" 'recenter-top-bottom)
     (define-key view-mode-map "a" 'ace-jump-mode)))

;; http://irreal.org/blog/?p=1536
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)

(require-package 'key-chord)
(key-chord-mode 1)

(provide 'init-general)
