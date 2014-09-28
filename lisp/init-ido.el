(require-package 'flx)
(require-package 'flx-ido)
(require-package 'ido-vertical-mode)
(require-package 'ido-ubiquitous)
(require-package 'ido-at-point)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-auto-merge-work-directories-length -1
      ido-max-prospects 10
      ido-save-directory-list-file "~/.emacs.d/cache/ido.hist"
      ido-max-directory-size 100000)

(add-hook 'dired-mode-hook
          '(lambda ()
             (set (make-local-variable 'ido-use-filename-at-point) nil)))

(setq ido-file-extensions-order
      '(".org" ".txt" ".md" ".rst" ".tex" ".py" ".el" ".hs"))

(setq completion-ignored-extensions
      (append '(".pdf" ".out" ".log" ".fls" ".fdb" ".fdb_latexmk")
              completion-ignored-extensions))

;; Disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-ubiquitous-mode 1)
(ido-at-point-mode)

(key-chord-define-global ",b" 'ido-switch-buffer)

(provide 'init-ido)
