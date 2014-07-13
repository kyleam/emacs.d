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

(require-package 'dired-subtree)

(define-prefix-command 'km/dired-subtree-map)
(define-key km/dired-subtree-map "i" 'dired-subtree-insert)
(define-key km/dired-subtree-map "r" 'dired-subtree-remove)
(define-key km/dired-subtree-map "g" 'dired-subtree-revert)
(define-key km/dired-subtree-map "s" 'dired-subtree-narrow)
(define-key km/dired-subtree-map "u" 'dired-subtree-up)
(define-key km/dired-subtree-map "d" 'dired-subtree-down)
(define-key km/dired-subtree-map "n" 'dired-subtree-next-sibling)
(define-key km/dired-subtree-map "p" 'dired-subtree-previous-sibling)
(define-key km/dired-subtree-map "<" 'dired-subtree-beginning)
(define-key km/dired-subtree-map ">" 'dired-subtree-end)
(define-key km/dired-subtree-map "@" 'dired-subtree-mark-subtree)
(define-key km/dired-subtree-map "." 'dired-subtree-unmark-subtree)

(defun km/dired-subtree-bindings ()
  (local-set-key (kbd "C-c m s") 'km/dired-subtree-map))
(add-hook 'dired-mode-hook 'km/dired-subtree-bindings)

(provide 'init-dired)
