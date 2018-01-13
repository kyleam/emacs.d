;;; km-hydra.el --- Hydra definitions

;; Copyright (C) 2012-2018 Kyle Meyer <kyle@kyleam.com>

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

(require 'hydra)

(defhydra hydra-org-link-edit ()
    "Org Link Edit"
    ("j" org-link-edit-forward-slurp "forward slurp")
    ("k" org-link-edit-forward-barf "forward barf")
    ("u" org-link-edit-backward-slurp "backward slurp")
    ("i" org-link-edit-backward-barf "backward barf")
    ("l" km/org-link-edit-slurp-link "slurp link" :color blue)
    ("q" nil "cancel"))

(defhydra hydra-smerge (:hint nil)
  "
_b_ keep base    _d_ diff     _n_ next
_m_ keep mine    _e_ ediff    _p_ previous
_o_ keep other   _h_ refine
_a_ keep all
\n"
  ("b" smerge-keep-base)
  ("m" smerge-keep-mine)
  ("o" smerge-keep-other)
  ("a" smerge-keep-all)
  ("n" smerge-next)
  ("p" smerge-prev)
  ("h" smerge-refine)
  ("e" smerge-ediff :color blue)
  ("d" (call-interactively
        (pcase (read-char-choice
                "< base-mine, > base-other, = mine-other"
                (list ?< ?> ?=))
          (?< #'smerge-diff-base-mine)
          (?> #'smerge-diff-base-other)
          (?= #'smerge-diff-mine-other))))
  ("l" recenter-top-bottom "recenter")
  ("u" undo "undo")
  ("q" nil "quit"))

(defhydra hydra-file-search-map (:hint nil :color blue)
  "
^^Grep             ^^Dired
^^------------     ^^------------------
_f_: grep-find     _d_: find-grep-dired
_g_: lgrep         _D_: find-dired
_G_: grep          _n_: find-name-dired
_r_: rgrep
_v_: vc-git-grep
_z_: zgrep
\n"
  ("f" grep-find)
  ("g" lgrep)
  ("G" grep)
  ("r" rgrep)
  ("v" vc-git-grep)
  ("z" zrgrep)

  ("d" find-grep-dired)
  ("D" find-dired)
  ("n" find-name-dired)

  ("q" nil "quit"))

;; Modified from https://github.com/abo-abo/hydra/wiki/Emacs
(defhydra hydra-outline-mode (:hint nil)
  "
  ^^Hide         ^^Show        ^^Move
_q_ sublevels  _a_ all       _u_ up
_t_ body       _e_ entry     _n_ next visible
_o_ other      _i_ children  _p_ previous visible
_c_ entry      _k_ branches  _f_ forward same level
_h_ leaves     _s_ subtree   _b_ backward same level
_d_ subtree

"
  ("q" hide-sublevels)
  ("t" hide-body)
  ("o" hide-other)
  ("c" hide-entry)
  ("h" hide-leaves)
  ("d" hide-subtree)

  ("a" show-all)
  ("e" show-entry)
  ("i" show-children)
  ("k" show-branches)
  ("s" show-subtree)

  ("u" outline-up-heading)
  ("n" outline-next-visible-heading)
  ("p" outline-previous-visible-heading)
  ("f" outline-forward-same-level)
  ("b" outline-backward-same-level)

  ("l" km/outline-jump-to-heading "jump" :color blue)
  ("m" outline-mark-subtree "mark" :color blue))

(defun km/hydra-outline-mode ()
  (interactive)
  (unless outline-minor-mode
    (outline-minor-mode))
  (hydra-outline-mode/body))

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

;; Multiple cursors hydra is modified from
;; https://github.com/abo-abo/hydra/wiki/multiple-cursors
(defhydra hydra-multiple-cursors (:hint nil)
  "
  ^^Up        ^^Down
_p_ Next    _n_ Next
_P_ Skip    _N_ Skip
_y_ Unmark  _u_ Unmark

"
  ("l" mc/edit-lines "edit lines" :exit t)
  ("a" mc/mark-all-like-this "mark all" :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("u" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("y" mc/unmark-previous-like-this))

(defhydra hydra-kmacro (:hint nil :exit t)
  "
  ^^Defining                ^^Applying
_?_ Query                 _c_ Call
_b_ Bind to key           _h_ Execute with Helm
_e_ Edit                  _o_ Call 2nd in ring
_E_ Step edit             _r_ Apply to region lines
_N_ Name last
_l_ Use recent strokes
_s_ Start defining
_x_ To register

  ^^Ring                    ^^Counter
_D_ Delete ring head      _+_ Add counter
_n_ Cycle next            _=_ Set counter
_p_ Cycle previous        _f_ Set format
_t_ Swap                  _i_ Insert counter
_v_ View

"
  ("+" kmacro-add-counter)
  ("=" kmacro-set-counter)
  ("D" kmacro-delete-ring-head)
  ("f" kmacro-set-format)
  ("i" kmacro-insert-counter)
  ("c" kmacro-end-and-call-macro :exit nil)
  ("o" kmacro-call-ring-2nd-repeat)
  ("e" kmacro-edit-macro)
  ("E" kmacro-step-edit-macro)
  ("h" helm-execute-kmacro)
  ("n" kmacro-cycle-ring-next :exit nil)
  ("p" kmacro-cycle-ring-previous :exit nil)
  ("t" kmacro-swap-ring)
  ("v" kmacro-view-macro-repeat)
  ("b" kmacro-bind-to-key)
  ("l" kmacro-edit-lossage)
  ("N" kmacro-name-last-macro)
  ("?" kbd-macro-query)
  ("r" apply-macro-to-region-lines)
  ("s" kmacro-start-macro)
  ("x" kmacro-to-register)
  ("q" nil "quit" :hint t))

(provide 'km-hydra)
;;; km-hydra.el ends here
