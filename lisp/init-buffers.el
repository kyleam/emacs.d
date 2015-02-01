
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

(global-set-key (kbd "C-x K") 'km/kill-buffer)

(key-chord-define-global ",d" 'km/save-and-kill-buffer)
(key-chord-define-global ",s" 'save-buffer)
(key-chord-define-global ",q" 'kill-this-buffer)

;; Replace `list-buffers' with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-buffers)
