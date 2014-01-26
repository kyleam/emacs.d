(require-package 'markdown-mode)

(setq-default fill-column 72)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun km/export-wrapped-text (arg)
  "Export the text in current buffer as wrapped text.
This is useful for preparing text in emacs and then exporting to
a wrapped buffer for pasting text (e.g., into a web form).

If region is active, export is restricted to the region. If ARG
is non-nil, the region is copied with `x-select-text'."
  (interactive "P")
  (let ((wrapped-buffer (get-buffer-create "*Wrapped export*"))
        beg end)
    (if (region-active-p)
        (progn (setq beg (region-beginning))
               (setq end (region-end)))
      (setq beg (point-min))
      (setq end (point-max)))
    (copy-to-buffer wrapped-buffer beg end)
    (switch-to-buffer-other-window wrapped-buffer)
    (while (not (eobp))
      (forward-paragraph)
      (forward-line -1)
      (unfill-paragraph)
      (forward-line 1))
    (when arg
      (x-select-text (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'init-text)
