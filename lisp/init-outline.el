
;; Modified from https://github.com/abo-abo/hydra/wiki/Emacs
(defhydra hydra-outline-mode (:hint nil)
  "
  ^^Hide         ^^Show        ^^Move
_q_ sublevels  _a_ all       _u_ up
_t_ body       _e_ entry     _n_ next visible
_o_ other      _i_ children  _p_ previous visible
_c_ entry      _k_ branches  _f_ forward same level
_l_ leaves     _s_ subtree   _b_ backward same level
_d_ subtree

"
  ("q" hide-sublevels)
  ("t" hide-body)
  ("o" hide-other)
  ("c" hide-entry)
  ("l" hide-leaves)
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

  ("m" outline-mark-subtree "mark" :color blue))

(defun km/hydra-outline-mode ()
  (interactive)
  (unless outline-minor-mode
    (outline-minor-mode))
  (hydra-outline-mode/body))

(global-set-key (kbd "C-c n") 'km/hydra-outline-mode)

(provide 'init-outline)
