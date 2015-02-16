;;; Frames and windows

(defun km/clone-indirect-buffer-other-window-and-widen ()
  "Clone as indirect buffer and then widen."
   (interactive)
   (call-interactively #'clone-indirect-buffer-other-window)
   (widen))

;; From prelude
(defun km/swap-windows ()
  "Swap 2 windows."
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
    (user-error "Can only toggle a window split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))
    (switch-to-buffer nil)))

(global-set-key (kbd "M-o") 'scroll-other-window)

(define-prefix-command 'km/window-map)
(global-set-key (kbd "C-c w") 'km/window-map)

(define-key km/window-map "f" 'make-frame)
(define-key km/window-map "l" 'km/switch-window-split)
(define-key km/window-map "r" 'winner-redo)
(define-key km/window-map "s" 'km/swap-windows)
(define-key km/window-map "u" 'winner-undo)

(key-chord-define-global "lq" 'winner-undo)

(define-key ctl-x-4-map "c" 'km/clone-indirect-buffer-other-window-and-widen)

(winner-mode 1)

(provide 'init-framewin)
