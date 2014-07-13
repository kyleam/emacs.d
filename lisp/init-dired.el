(put 'dired-find-alternate-file 'disabled nil)

(require 'dired-x)

;; .git is present as part of `dired-omit-extensions', but this seems to
;; only be taken into account if a non-exension part exists.
(setq dired-omit-files
      (concat dired-omit-files
              "\\|^\\.git$\\|^\\.gitignore$"
              "\\|^__pycache__$\\|^\\.snakemake$"))

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
;; This overrides the binding for `list-directory'.
(global-set-key (kbd "C-x C-d") 'km/dired-switch-to-buffer)

(defun km/org-open-dired-marked-files (&optional in-emacs)
  (interactive "P")
  (let* ((files (dired-get-marked-files))
         (num-files (length files)))
    (when (or (< num-files 5)
              (yes-or-no-p (format "Open %s files?" num-files)))
      (--each files (org-open-file it in-emacs)))))

(after 'org
  ;; This overrides `dired-find-file', which is also bound to "f".
  (define-key dired-mode-map "e" 'km/org-open-dired-marked-files))

(require-package 'dired-narrow)

(define-prefix-command 'km/dired-narrow-map)
(define-key km/dired-narrow-map "n" 'dired-narrow)
(define-key km/dired-narrow-map "f" 'dired-narrow-fuzzy)
(define-key km/dired-narrow-map "r" 'dired-narrow-regexp)

(defun km/dired-narrow-bindings ()
  (local-set-key (kbd "C-c m n") 'km/dired-narrow-map))
(add-hook 'dired-mode-hook 'km/dired-narrow-bindings)

(define-key dired-mode-map "/" 'dired-narrow)

(provide 'init-dired)
