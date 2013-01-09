;; check km-editor.el if don't find something in here

;; to customize font
(setq default-frame-alist '((font . "Droid Sans Mono-9")))

(require 'font-lock)
(global-font-lock-mode t)
(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))

;; splash screen
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

;; y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; line info
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
