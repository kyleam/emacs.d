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
(setq dired-listing-switches "-alh"
      dired-dwim-target t)

(setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "zathura")))

(setq dired-recursive-deletes t
      dired-recursive-copies t)

(defun km/dired-copy-project-filename-as-kill ()
  "Copy names of marked project files into kill ring.
This is similar to `dired-copy-filename-as-kill', but the leading
path is always relative to `projectile-project-root'."
  (interactive)
  (km/dired-copy-filename-relative-to-directory
   (projectile-project-root)))

(defun km/dired-copy-filename-relative-to-directory (directory)
  "Like `dired-copy-filename-as-kill', but the filename is always
relative to DIRECTORY."
  (let* ((string
          (mapconcat 'identity
                     (--map (file-relative-name it directory)
                            (dired-get-marked-files t))
                     " ")))
    (if (eq last-command 'kill-region)
        (kill-append string nil)
      (kill-new string))
    (message "%s" string)))

(defun km/dired-copy-relative-filename-as-kill ()
  "Copy names of marked files into kill ring.
This is similar to `dired-copy-filename-as-kill', but the leading
path is always relative to the `default-directory' of the other
window."
  (interactive)
  (km/dired-copy-filename-relative-to-directory
   (km/other-default-directory)))

(defun km/other-default-directory ()
  "Get `default-directory' for result of `(other-window 1)'."
  (save-window-excursion
    (other-window 1)
    default-directory))

(define-prefix-command 'km/dired-copy-filename-map)
(define-key km/dired-copy-filename-map "o"
  'km/dired-copy-relative-filename-as-kill)
(define-key km/dired-copy-filename-map "w" 'dired-copy-filename-as-kill)
(after 'projectile
  (define-key km/dired-copy-filename-map "p"
    'km/dired-copy-project-filename-as-kill))

;; This overrides the default binding for `dired-copy-filename-as-kill'.
(define-key dired-mode-map "w" 'km/dired-copy-filename-map)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(define-key dired-mode-map (kbd "C-c C-b") 'dired-up-directory)

(defun km/dired-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (km/dired-completing-buffer)))

(defun km/dired-switch-to-buffer-other-window ()
  (interactive)
  (pop-to-buffer (km/dired-completing-buffer)))

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

(define-prefix-command 'km/dired-narrow-prefix-map)
(define-key km/dired-narrow-prefix-map "n" 'dired-narrow)
(define-key km/dired-narrow-prefix-map "f" 'dired-narrow-fuzzy)
(define-key km/dired-narrow-prefix-map "r" 'dired-narrow-regexp)

(define-key dired-mode-map "/" 'dired-narrow)

(define-prefix-command 'km/dired-subtree-prefix-map)
(define-key km/dired-subtree-prefix-map "i" 'dired-subtree-insert)
(define-key km/dired-subtree-prefix-map "r" 'dired-subtree-remove)
(define-key km/dired-subtree-prefix-map "g" 'dired-subtree-revert)
(define-key km/dired-subtree-prefix-map "s" 'dired-subtree-narrow)
(define-key km/dired-subtree-prefix-map "u" 'dired-subtree-up)
(define-key km/dired-subtree-prefix-map "d" 'dired-subtree-down)
(define-key km/dired-subtree-prefix-map "n" 'dired-subtree-next-sibling)
(define-key km/dired-subtree-prefix-map "p" 'dired-subtree-previous-sibling)
(define-key km/dired-subtree-prefix-map "<" 'dired-subtree-beginning)
(define-key km/dired-subtree-prefix-map ">" 'dired-subtree-end)
(define-key km/dired-subtree-prefix-map "@" 'dired-subtree-mark-subtree)
(define-key km/dired-subtree-prefix-map "." 'dired-subtree-unmark-subtree)

(define-prefix-command 'km/dired-prefix-map)

(define-key km/dired-prefix-map "n" 'km/dired-narrow-prefix-map)
(define-key km/dired-prefix-map "s" 'km/dired-subtree-prefix-map)

(define-key dired-mode-map (kbd "C-c m") 'km/dired-prefix-map)

(provide 'init-dired)
