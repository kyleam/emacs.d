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
(defadvice vc-git-grep (after hide-grep-header activate) (hide-grep-header))
