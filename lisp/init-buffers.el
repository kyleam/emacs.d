
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

(global-set-key (kbd "C-x K") 'kill-buffer-and-window)

(key-chord-define-global ",d" 'km/save-and-kill-buffer)
(key-chord-define-global ",s" 'save-buffer)
(key-chord-define-global ",q" 'kill-this-buffer)

;; Replace `list-buffers' with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-buffers)
