;; Make cite key have form <last author last name><year><first word>.
(setq bibtex-autokey-year-length 4
      bibtex-autokey-titleword-length nil
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titlewords 1
      bibtex-autokey-year-title-separator ""
      bibtex-autokey-titleword-ignore '("A" "An" "On" "The"  "[0-9].*"))

(setq bibtex-align-at-equal-sign t)  ; Used by `bibtex-fill-entry'.

(after 'bibtex
  (setq bibtex-entry-format
        (append '(realign sort-fields) bibtex-entry-format)))

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
All words except those in `km/bibtex-unimportant-title-words' are
capitalized."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (goto-char (car (cdr (bibtex-search-forward-field "title" t))))
    (while (not (looking-at "},"))
      ;; Not using `forward-word' because I want to capture character
      ;; before word. If "-" or "{", the word should not be capitalized.
      (re-search-forward "\\(.\\)[a-z]+")
      (let ((before-word (match-string-no-properties 1))
            (word (thing-at-point 'word)))
        (unless (or (member before-word '("-" "{"))
                    (member word km/bibtex-unimportant-title-words))
          (backward-word)
          (capitalize-word 1))))))

(defadvice bibtex-clean-entry (before convert-to-title-case activate)
  (km/bibtex-use-title-case))

(defun km/browse-doi (doi)
  "Open DOI in browser.
The link is opened using the settings of `org-doi-server-url'.
When called interactively, DOI is taken from the text under
point."
  (interactive (list (km/doi-at-point)))
  (browse-url (org-link-escape-browser (concat org-doi-server-url doi))))

(defun km/doi-at-point ()
  "Return DOI at point."
  (save-excursion
    (re-search-backward "[ \t\n]" nil t)
    (re-search-forward "\\(doi:[ \t\n]*\\)*\\([-./A-z0-9]+\\)[.; \t\n]" nil t)
    (--if-let (match-string-no-properties 2)
        (s-chop-suffix "." it)
      (error "No DOI found"))))

(provide 'init-bib)
