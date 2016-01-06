;;; init-framewin.el --- Frame and window configuration

;; Copyright (C) 2012-2016 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/emacs.d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

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

(defhydra hydra-window-map (:hint nil)
  "
_l_: Switch split   _r_: Winner redo   _o_: Scroll other
_s_: Swap           _u_: Winner undo   _i_: Scroll other down
\n"
  ("l" km/switch-window-split)
  ("s" km/swap-windows)

  ("r" winner-redo)
  ("u" winner-undo)

  ("o" scroll-other-window)
  ("i" scroll-other-window-down)

  ("f" make-frame "new frame" :color blue)
  ("q" nil "quit"))

(global-set-key (kbd "C-c w") 'hydra-window-map/body)

(key-chord-define-global "lq" 'winner-undo)

(define-key ctl-x-4-map "c" 'km/clone-indirect-buffer-other-window-and-widen)

(winner-mode 1)

(provide 'init-framewin)
;;; init-framewin.el ends here
