
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun km/export-wrapped-text (arg)
  "Export the text in current buffer as wrapped text.

This is useful for preparing text in emacs and then exporting to
a wrapped buffer for pasting text (e.g., into a web form).

If region is active, restrict export to the region. If ARG is
non-nil, copy the region with `x-select-text'."
  (interactive "P")
  (let ((wrapped-buffer (get-buffer-create "*Wrapped export*"))
        beg end)
    (if (use-region-p)
        (progn (setq beg (region-beginning))
               (setq end (region-end)))
      (setq beg (point-min))
      (setq end (point-max)))
    (copy-to-buffer wrapped-buffer beg end)
    (switch-to-buffer-other-window wrapped-buffer)
    (while (not (eobp))
      (forward-paragraph)
      (forward-line -1)
      (km/unfill-paragraph)
      (forward-line 1))
    (when arg
      (x-select-text (buffer-substring-no-properties (point-min) (point-max))))))

(defun km/columnify-file (delim)
  "Separate current file on DELIM using column program.

By default, DELIM is set to \",\". With a single prefix argument,
use whitespace as the delimiter. With two prefix arguments,
prompt for a delimiter.

If a columnified buffer already exists, just switch to it."
  (interactive (list (cond ((not current-prefix-arg) ",")
                           ((> (prefix-numeric-value current-prefix-arg) 4)
                            (read-string "Delimiter: "))
                           (t nil))))
  (unless buffer-file-name
    (user-error "Buffer not visiting a file"))
  (let* ((output-buffer-name (concat "*cols: " (buffer-name) "*"))
         (output-buffer (get-buffer output-buffer-name))
         (fname (file-relative-name buffer-file-name))
         (args (cons "--table"
                     (and delim (list "--separator" delim)))))
    (unless output-buffer
      (setq output-buffer (get-buffer-create output-buffer-name))
      (apply #'call-process "column" fname output-buffer nil args))
    (switch-to-buffer output-buffer)))

(provide 'init-text)
