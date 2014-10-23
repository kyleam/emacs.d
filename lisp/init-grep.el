(autoload 'vc-git-grep "vc-git"
  "Run git grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.")

;; http://stackoverflow.com/questions/16122801/
;; remove-header-information-from-rgrep-grep-output-in-emacs
(defun hide-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defadvice grep (after hide-grep-header activate) (hide-grep-header))
(defadvice rgrep (after hide-grep-header activate) (hide-grep-header))
(defadvice lgrep (after hide-grep-hxoeader activate) (hide-grep-header))
(defadvice grep-find (after hide-grep-header activate) (hide-grep-header))
(after 'vc-git
  (defadvice vc-git-grep (after hide-grep-header activate) (hide-grep-header)))

(key-chord-define-global ",z" 'rgrep)

(provide 'init-grep)
