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

;; line info
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; set location of custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")

(setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "chromium")

(put 'dired-find-alternate-file 'disabled nil)

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; make scripts executable at save
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(setq-default fill-column 72)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; (require 'ess-site)
(add-hook 'ess-mode-hook (lambda ()
                           (setq ess-indent-level 4)))

(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 4)))

;; make whitespace-mode use just basic coloring
;; http://ergoemacs.org/emacs/whitespace-mode.html
(setq whitespace-style (quote
                        (spaces tabs newline space-mark
                                tab-mark newline-mark)))

(delete-selection-mode t) ;; write over selected text
(transient-mark-mode t)
;; share clipboard with system
(setq x-select-enable-clipboard t)

(blink-cursor-mode -1)

(put 'narrow-to-region 'disabled nil)
