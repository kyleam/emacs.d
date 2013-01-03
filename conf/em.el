;; general emacs settings
(server-start)

(setq-default fill-column 72)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; to customize font
(setq default-frame-alist '((font . "Droid Sans Mono-9")))

(setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "google-chrome")
;; ess
(require 'ess-site)
(add-hook 'ess-mode-hook (lambda ()
                           (setq ess-indent-level 4)))

;;(require 'dired+)

;; shell scripts
(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 4)))

;; set location of custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; make whitespace-mode use just basic coloring
;; http://ergoemacs.org/emacs/whitespace-mode.html
(setq whitespace-style (quote
                        (spaces tabs newline space-mark
                                tab-mark newline-mark)))
;; y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(delete-selection-mode t) ;; write over selected text
(transient-mark-mode t)
;; share clipboard with system
(setq x-select-enable-clipboard t)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))


(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; line info
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)


;; ido mode
(ido-mode t)
(setq ido-enable-prefix nil
      ido-everywhere t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file "~/.emacs.d/cache/ido.hist")

;; recent files
(setq recentf-save-file "~/.emacs.d/cache/recentf"
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)
;; from prelude
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")
