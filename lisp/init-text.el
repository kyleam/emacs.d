(require-package 'markdown-mode)

(setq-default fill-column 72)

(add-hook 'text-mode-hook '(lambda ()
                             (abbrev-mode 1)
                             (turn-on-auto-fill)))

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
  (let* ((bufname (buffer-name))
         (output-buffer-name (concat "*cols: " bufname "*"))
         (output-buffer (get-buffer output-buffer-name))
         (fname (file-relative-name buffer-file-name))
         (col-args '("-t")))
    (unless output-buffer
      (setq output-buffer (get-buffer-create output-buffer-name))
      (when delim
        (add-to-list 'col-args (format "-s'%s'" delim)))
      (apply 'call-process "column" fname output-buffer nil
             col-args))
    (switch-to-buffer output-buffer)))

(provide 'init-text)
