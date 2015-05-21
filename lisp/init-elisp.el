
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(after 'paredit
  (define-key paredit-mode-map (kbd "C-,")
    (defhydra hydra-paredit ()
      "
_a_: beginning of defun  _j_: forward slurp
_f_: forward             _k_: forward barf
_b_: backward            _u_: backward slurp
_p_: backward down       _i_: backward barf
_n_: forward up
_d_: forward down        _w_: copy as kill
_v_: backward up         _q_: indent sexp

_c_: convolute sexp      _l_: split sexp
_r_: raise               _o_: join sexp
_s_: splice sexp
_R_: wrap round
"
      ("j" paredit-forward-slurp-sexp nil)
      ("k" paredit-forward-barf-sexp nil)
      ("u" paredit-backward-slurp-sexp nil)
      ("i" paredit-backward-barf-sexp nil)

      ("a" beginning-of-defun nil)
      ("f" paredit-forward nil)
      ("b" paredit-backward nil)
      ("p" paredit-backward-down nil)
      ("n" paredit-forward-up nil)
      ("d" paredit-forward-down nil)
      ("v" paredit-backward-up nil)

      ("c" paredit-convolute-sexp nil)
      ("r" paredit-raise-sexp nil)
      ("s" paredit-splice-sexp nil)
      ("R" paredit-wrap-round nil)

      ("l" paredit-split-sexp nil)
      ("o" paredit-join-sexps nil)

      ("w" paredit-copy-as-kill nil)

      ("q" indent-pp-sexp nil)))

  ;; Don't let `paredit-splice-sexp' override default 'M-s' map.
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-p") 'paredit-splice-sexp))

(provide 'init-elisp)
