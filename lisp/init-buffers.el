
(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)

(setq ibuffer-expert t
      ibuffer-restore-window-config-on-quit t
      ibuffer-show-empty-filter-groups nil)

(defun km/save-and-kill-buffer ()
  "Save current buffer and then kill it."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

(defun km/kill-buffer (&optional arg)
  "Kill this buffer.
With single C-u, prompt for buffer to kill.  With double C-u,
kill this buffer and the window."
  (interactive "P")
  (cond
   ((not arg)
    (kill-buffer))
   ((equal arg '(16))
    (kill-buffer-and-window))
   (t
    (call-interactively #'kill-buffer))))

(defun km/save-buffers ()
  "Run `save-some-buffers', but don't ask to save the current buffer.
`save-some-buffers' is called interactively."
  (interactive)
  (let* ((base-buf (buffer-base-buffer))
         (buf (or base-buf (current-buffer)))
         (buf-file (buffer-file-name buf)))
    (when (and (buffer-live-p buf)
               (buffer-modified-p buf)
               buf-file)
      (with-current-buffer buf
        (save-buffer))))
  (call-interactively #'save-some-buffers))

(global-set-key (kbd "C-x K") 'km/kill-buffer)

(key-chord-define-global "js" 'km/save-buffers)

;; Replace `list-buffers' with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-buffers)
