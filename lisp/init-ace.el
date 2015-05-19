;;; Ace Jump

(setq ace-jump-mode-scope 'frame)

(key-chord-define-global "jf" 'ace-jump-mode)

(setq ace-jump-mode-move-keys (loop for i from ?a to ?z collect i))

;;; Ace Link

(defun km/ace-link-dired ()
  "Ace jump to files in dired buffers."
  (interactive)
  (let ((res (avy--with-avy-keys km/ace-link-dired
               (avy--process
                (km/ali--dired-collect-references)
                #'avy--overlay-pre))))
    (when res
      (goto-char res)
      (org-open-file (dired-get-filename)))))

(defun km/ali--dired-collect-references ()
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (while (< (point) end)
        (--when-let (dired-next-line 1)
          (push it points)))
      (nreverse points))))

(ace-link-setup-default)
(after 'org
 (define-key org-mode-map (kbd "C-c m o") 'ace-link-org))
(after 'dired
  ;; This overrides the binding for `dired-find-file-other-window'.
  (define-key dired-mode-map "o" 'km/ace-link-dired)
  (define-key dired-mode-map "r" 'dired-find-file-other-window))

;;; Ace Window

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-scope 'frame)

(defun km/ace-window (arg)
  "Run `ace-window', swapping single and double C-u's."
  (interactive "p")
  (cl-case arg
    (4  (setq arg 16))
    (16 (setq arg 4)))
  (ace-window arg))

(key-chord-define-global "jw" 'km/ace-window)

(provide 'init-ace)
