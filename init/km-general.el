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
          browse-url-generic-program "google-chrome")

(put 'dired-find-alternate-file 'disabled nil)
;;(require 'dired+)
