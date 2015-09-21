;; Make cite key have form <last author last name><year><first word>.
(setq bibtex-autokey-titlewords 1
      bibtex-autokey-titleword-ignore '("A" "An" "On" "The"  "[0-9].*")
      bibtex-autokey-titleword-length nil
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-year-length 4
      bibtex-autokey-year-title-separator "")

(setq bibtex-align-at-equal-sign t)  ; Used by `bibtex-fill-entry'.

(after 'bibtex
  (setq bibtex-entry-format
        (append '(realign whitespace last-comma delimiters sort-fields)
                bibtex-entry-format)))

(add-hook 'bibtex-clean-entry-hook 'km/bibtex-use-title-case)
(add-hook 'bibtex-clean-entry-hook 'km/bibtex-single-space-author-list)
(add-hook 'bibtex-clean-entry-hook 'km/bibtex-pages-use-double-hyphen)
(add-hook 'bibtex-clean-entry-hook 'km/bibtex-remove-doi-leader)
(add-hook 'bibtex-clean-entry-hook 'km/bibtex-set-coding-system)
(add-hook 'bibtex-clean-entry-hook 'km/bibtex-remove-entry-space)

(defvar km/bibtex-unimportant-title-words
  '("a" "aboard" "about" "above" "absent" "across" "after" "against"
    "along" "alongside" "amid" "amidst" "among" "amongst" "an" "and"
    "around" "as" "aslant" "astride" "at" "athwart" "atop"
    "barring" "before" "behind" "below" "beneath" "beside" "besides" "between"
    "beyond" "but" "by" "despite" "down" "during" "except" "failing"
    "following" "for" "from" "in" "inside" "into" "like"
    "mid" "minus" "near" "next" "nor" "notwithstanding" "of" "off"
    "on" "onto" "opposite" "or" "out" "outside" "over" "past"
    "per" "plus" "regarding" "round" "save" "since" "so" "than"
    "the" "through" "throughout" "till" "times" "to" "toward" "towards"
    "under" "underneath" "unlike" "until" "up" "upon" "via" "vs."
    "when" "with" "within" "without" "worth" "yet")
  "Words to ignore when running `km/bibtex-use-title-case'.
These are taken from
http://lanecc.libguides.com/content.php?pid=38483&sid=295540 and
have only been modified to remove duplicates. This means that
there are some unlikely words in there, but you never know when
the next article you read will have \"athwart\" in the title.")

(defun km/bibtex-use-title-case ()
  "Convert title of current BibTeX entry to title case.
Change words in `km/bibtex-unimportant-title-words' to lower
case, unless the word is the first word in the title.  Capitalize
all other words unless they are protected by brackets."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((text-bounds (cdr (bibtex-search-forward-field "title" t)))
           (beg (car text-bounds))
           (end (cadr text-bounds)))
      (goto-char (1- beg))
      (while (re-search-forward "\\(\\W\\)\\(\\w+\\)\\(\\W\\)" end t)
        (cond
         ((and (string= (match-string 1) "{")
               (string= (match-string 3) "}"))
          ;; Go to previous character in case '}' is within the word.
          (backward-char))
         ;; Leave commands alone.
         ((string= (match-string 1) "\\"))
         ;; Capitalize the first word of the title.  This will fail if
         ;; there is a space after '{'.
         ((= (match-beginning 1) beg)
          (backward-word)
          (capitalize-word 1))
         ;; Subword is separated by '-' or '{'.
         ((or (string= (match-string 1) "-")
              (string= (match-string 1) "}"))
          (backward-word)
          (downcase-word 1))
         (t
          (backward-word)
          (if (member (downcase (match-string-no-properties 2))
                      km/bibtex-unimportant-title-words)
              (downcase-word 1)
            (capitalize-word 1))))))))

(defun km/bibtex-single-space-author-list ()
  "Convert multiple spaces in author list to single space."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((text-bounds (cdr (bibtex-search-forward-field "author" t)))
           (beg (car text-bounds))
           (end (cadr text-bounds)))
      (when text-bounds
        (goto-char beg)
        (while (re-search-forward "\\(\\s-+\\) and" end t)
          (replace-match "" nil nil nil 1))
        (goto-char beg)
        (fill-paragraph)))))

(defun km/bibtex-set-coding-system ()
  (set-buffer-file-coding-system 'utf-8))

(defun km/bibtex-remove-entry-space ()
  "Remove space in entry header.
For example, convert

  @article {

to

  @article{"
  (save-excursion
    (bibtex-beginning-of-entry)
    (when (looking-at "@\\w+\\(\\s-+\\)")
      (replace-match "" nil nil nil 1))))

(defun km/bibtex-pages-use-double-hyphen ()
  "Use double hyphen for page range."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((text-bounds (cdr (bibtex-search-forward-field "pages" t)))
           (beg (car text-bounds))
           (end (cadr text-bounds)))
      (when text-bounds
        (goto-char beg)
        (and (re-search-forward "[^A-z0-9]*-[^A-z0-9]*" end t)
             (replace-match "--"))))))

(defun km/bibtex-remove-doi-leader ()
  "Remove leading part (http:...) of doi URL."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((text-bounds (cdr (bibtex-search-forward-field "doi" t)))
           (beg (car text-bounds))
           (end (cadr text-bounds)))
      (when text-bounds
        (goto-char beg)
        (and (re-search-forward "http://dx.doi.org/" end t)
             (replace-match ""))))))

(defun km/browse-doi (doi)
  "Open DOI in browser.
When called interactively, take the DOI from the text under
point.  The link is opened using the settings of
`org-doi-server-url'."
  (interactive (list (km/doi-at-point)))
  (browse-url (org-link-escape-browser (concat org-doi-server-url doi))))

(defun km/copy-doi-as-kill ()
  "Copy DOI at point to kill ring."
  (interactive)
  (-when-let (doi (km/doi-at-point))
    (kill-new (message (concat "doi:" doi)))))

(defun km/doi-at-point ()
  "Return DOI at point."
  (save-excursion
    (skip-chars-backward "-.A-z0-9/")
    (and (looking-at "\\(doi:[ \t\n]*\\)*\\([-./A-z0-9]+[A-z0-9]\\)\\b")
         (match-string-no-properties 2))))

(provide 'init-bib)
