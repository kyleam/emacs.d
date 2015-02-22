
(setq save-abbrevs 'silently)

(define-abbrev-table 'typo-abbrev-table nil)
(abbrev-table-put global-abbrev-table :parents
                  (cons typo-abbrev-table
                        (abbrev-table-get global-abbrev-table :parents)))

(defun km/abbrev-add-case-global ()
  "Define lower abbreviation for the word before point.
Like `add-global-abbrev', but always make the abbreviation the
lower case variant of the word before point."
  (interactive)
  ;; Modified from `add-abbrev'.
  (let* ((table global-abbrev-table)
         (exp (buffer-substring-no-properties
               (point)
               (save-excursion (forward-word -1) (point))))
         (name (downcase exp)))
    (when (or (not (abbrev-expansion name table))
              (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                                name (abbrev-expansion name table))))
      (define-abbrev table name exp))))

(defun km/abbrev-inverse-add-uppercase-global ()
  "Define uppercase expansion for the word before point.
Like `inverse-add-global-abbrev', but always use the lower case
version of the word before point as the abbreviation and the
upper case version as the expansion."
  (interactive)
  ;; Modified from `inverse-add-abbrev'.
  (let* ((table global-abbrev-table)
         (end (point))
         (start (save-excursion (forward-word -1) (point)))
         (name (downcase (buffer-substring-no-properties start end)))
         (exp (upcase name)))
    (when (or (not (abbrev-expansion name table))
              (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                                name (abbrev-expansion name table))))
      (define-abbrev table name exp)
      (save-excursion
        (goto-char end)
        (expand-abbrev)))))

(define-key abbrev-map "c" 'km/abbrev-add-case-global)
(define-key abbrev-map "iu" 'km/abbrev-inverse-add-uppercase-global)

(provide 'init-abbrev)
