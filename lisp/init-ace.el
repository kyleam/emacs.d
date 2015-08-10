;;; Avy

(require 'avy)

(defun km/avy-goto-subword-1 (char arg)
  "Like `avy-goto-subword-1', but don't consider invisible text."
  (interactive (list (read-char "char: ") current-prefix-arg))
  (avy--with-avy-keys avy-goto-subword-1
    (let ((char (downcase char)))
      (avy-goto-subword-0
       arg (lambda () (and (not (invisible-p (point)))
                           (eq (downcase (char-after)) char)))))))

(key-chord-define-global "jf" 'km/avy-goto-subword-1)

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

(defun km/ace-link-widget ()
  "Press a widget that is visible in the current buffer.
This can be used in place of `ace-link-gnus' and has the
advantage of working for gwene buffers in addition to normal mail
buffers because it doesn't rely on the 'gnus-string' text
property."
  (interactive)
  (when (eq major-mode 'gnus-summary-mode)
    (gnus-summary-widget-forward 1))
  (let ((res (avy--with-avy-keys km/ace-link-widget
               (avy--process
                (km/ali--widget-collect-references)
                #'avy--overlay-post))))
    (when res
      (goto-char (1+ res))
      (widget-button-press (point)))))

(defun km/ali--widget-collect-references ()
  "Collect the positions of visible widgets in buffer."
  (require 'wid-edit)
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (setq pt (point))
        (while (progn (ignore-errors (widget-forward 1))
                      (> (point) pt))
          (setq pt (point))
          (push (point) candidates))
        (nreverse candidates)))))

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
