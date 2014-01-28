(require-package 'flx)
(require-package 'flx-ido)

(setq ido-enable-prefix nil
      ido-everywhere t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file "~/.emacs.d/cache/ido.hist"
      ido-max-directory-size 100000)

;; Disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(ido-mode 1)
(flx-ido-mode 1)

;; Recent files
(setq recentf-save-file "~/.emacs.d/cache/recentf"
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; From prelude
(defun km/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(key-chord-define-global ",r" 'km/recentf-ido-find-file)
(key-chord-define-global ",b" 'ido-switch-buffer)

(provide 'init-ido)
