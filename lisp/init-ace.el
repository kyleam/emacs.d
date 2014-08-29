(require-package 'ace-jump-mode)
(require-package 'ace-link)
(require-package 'ace-window)

;;; Ace Jump

(key-chord-define-global ";a" 'ace-jump-mode)

(setq ace-jump-mode-scope 'frame)

;;; Ace Link

(ace-link-setup-default)

(after 'org
 (define-key org-mode-map (kbd "C-c m o") 'ace-link-org))

(defun km/ace-link-dired ()
  "Ace jump to files in dired buffers."
  (interactive)
  (ali-generic
      (km/ali--dired-collect-references)
    (org-open-file (dired-get-filename))))

(defun km/ali--dired-collect-references ()
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (while (< (point) end)
        (-when-let (pos (dired-next-line 1))
          (push pos points)))
      (nreverse points))))

(after 'dired
  ;; This overrides the binding for `dired-find-file-other-window', which
  ;; is rebound to 'r'.
  (define-key dired-mode-map "o" 'km/ace-link-dired)
  (define-key dired-mode-map "r" 'dired-find-file-other-window))

;;; Ace Window

(define-key km/window-map "a" 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-scope 'global)

(key-chord-define-global ",w" 'ace-window)

(provide 'init-ace)
