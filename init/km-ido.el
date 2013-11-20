(require 'flx-ido)
(ido-mode t)

(setq ido-enable-prefix nil
      ido-everywhere t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file "~/.emacs.d/cache/ido.hist")

(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(setq ido-max-directory-size 100000)

;; recent files
(setq recentf-save-file "~/.emacs.d/cache/recentf"
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; from prelude
(defun km/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
