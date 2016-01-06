;;; init-outline.el --- Outline mode configuration

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

(defun km/outline--parent-levels (n)
  "Return list of parent levels.
4 -> (1 2 3)
1 -> nil"
  (unless (> n 0)
    (user-error "N must be positive"))
  (let (parents)
    (while (/= n 1)
      (setq n (1- n))
      (push n parents))
    parents))

(defun km/outline-imenu-create-index ()
  "Generate Imenu index for headings.
This assumes that `outline-regexp' marks the characters that
indicate a heading, but not the text following it.  The heading
text is taken as everything on the line after the
`outline-regexp' match (except for any leading whitespace)."
  (goto-char (point-min))
  (let (path-alist                      ; ((level . heading))
        index)
    (while (re-search-forward (concat "^" outline-regexp) nil t)
      (let* ((head (replace-regexp-in-string
                    "\\`\\s-*" ""
                    (buffer-substring-no-properties (point) (point-at-eol))))
             (head-level (save-excursion (beginning-of-line)
                                         (funcall outline-level)))
             (parent-levels (km/outline--parent-levels head-level))
             (parent-heads
              (delq nil (mapcar (lambda (n) (cdr (assoc n path-alist)))
                                parent-levels)))
             (target (concat (mapconcat #'identity
                                        (append parent-heads
                                                (list head))
                                        " / "))))
        (unless (string= head "")
          (push (cons target (save-excursion (beginning-of-line) (point-marker)))
                index))
        (push (cons head-level head) path-alist)))
    (nreverse index)))

(defun km/outline-jump-to-heading ()
  "Jump to heading specified by `outline-regexp'."
  (interactive)
  (let ((imenu-create-index-function #'km/outline-imenu-create-index)
        helm-cached-imenu-tick
        helm-cached-imenu-alist
        helm-cached-imenu-candidates)
    (call-interactively #'helm-imenu)))

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

(global-set-key (kbd "C-c n") 'km/hydra-outline-mode)

(provide 'init-outline)
;;; init-outline.el ends here
