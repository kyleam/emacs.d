(put 'dired-find-alternate-file 'disabled nil)

(require 'dired-x)

;; .git is present as part of `dired-omit-extensions', but this
;; seems to only be taken into account if the a non-exension part
;; exists.
(setq dired-omit-files
      (concat dired-omit-files "\\|\\.git$\\|\\.gitignore$\\|__pycache__"))

(defvar km/latex-omit-extensions '(".aux"
                                   ".fdb_latexmk"
                                   ".fls"
                                   ".log"
                                   ".nav"
                                   ".out"
                                   ".snm")
  "Intermediate LaTeX files")

(setq dired-omit-extensions
      (append dired-omit-extensions km/latex-omit-extensions))

(setq-default dired-omit-mode t)
(setq dired-listing-switches "-alh")

(setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "zathura")))

(defun km/dired-switch-to-buffer ()
  (interactive)
  (let ((buffer-name (km/dired-completing-buffer)))
    (switch-to-buffer buffer-name)))

(defun km/dired-switch-to-buffer-other-window ()
  (interactive)
  (let ((buffer-name (km/dired-completing-buffer)))
    (pop-to-buffer buffer-name)))

(defun km/dired-completing-buffer ()
  (ido-completing-read "Dired buffer: "
                       (-map 'buffer-name (km/dired-buffer-list))))

(defun km/dired-buffer-list ()
  (--filter (with-current-buffer it
              (derived-mode-p 'dired-mode))
            (buffer-list)))

(define-key ctl-x-4-map "D" 'km/dired-switch-to-buffer-other-window)
(global-set-key (kbd "C-x D") 'km/dired-switch-to-buffer)

(provide 'init-dired)
