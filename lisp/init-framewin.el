;;; Frames and windows

;; From prelude
(defun km/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun km/switch-window-split ()
  "If the window is split vertically, split it horizontally or vice versa.
Assumes that the window is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a window split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))
    (switch-to-buffer nil)))

(define-prefix-command 'window-map)
(global-set-key (kbd "C-c w") 'window-map)

(define-key window-map "s" 'km/swap-windows)
(define-key window-map "l" 'km/switch-window-split)

(global-set-key (kbd "M-o") 'other-window)

(defadvice clone-indirect-buffer-other-window
  (after clone-indirect-and-widen activate)
  "Widen after cloning an indirect buffer."
  (widen))

(provide 'init-framewin)
