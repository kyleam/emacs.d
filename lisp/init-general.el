(setq default-frame-alist '((font . "Droid Sans Mono-9")))

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Line info
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Set location of custom.el.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Store all backup and autosave files in tmp dir.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Revert buffers automatically when underlying files are changed
;; externally.
(global-auto-revert-mode t)

;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")

(setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "chromium")

(setq x-select-enable-primary t)

;; Make scripts executable at save.
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(setq-default fill-column 72)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Make whitespace-mode use just basic coloring.
;; http://ergoemacs.org/emacs/whitespace-mode.html
(setq whitespace-style (quote
                        (spaces tabs newline space-mark
                                tab-mark newline-mark)))

(transient-mark-mode -1)

;; Share clipboard with system.
(setq x-select-enable-clipboard t)

(blink-cursor-mode -1)

(put 'narrow-to-region 'disabled nil)

(setq ispell-program-name "aspell")

(global-set-key (kbd "M-/") 'hippie-expand)
;; http://www.emacswiki.org/emacs/HippieExpand#toc9
(defadvice he-substitute-string (after he-paredit-fix activate)
  "Remove extra paren when expanding line in paredit."
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

(setq sentence-end-double-space nil)

(eval-after-load "view"
  '(progn
     (define-key view-mode-map "l" 'recenter-top-bottom)
     (define-key view-mode-map "a" 'ace-jump-mode)))

(setq shell-command-switch "-ic")

;; http://irreal.org/blog/?p=1536
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)

(provide 'init-general)
