;;; km-bib.el --- Bibliography configuration

;; Copyright (C) 2012-2016 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/emacs.d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'bibtex)
(require 'dash)
(require 'org)

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
    (let* ((bounds (bibtex-search-forward-field "title" t))
           (beg (bibtex-start-of-text-in-field bounds)))
      (goto-char (1- beg))
      (while (re-search-forward "\\(\\W\\)\\(\\w+\\)\\(\\W\\)"
                                (bibtex-end-of-text-in-field bounds) t)
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
    (let* ((bounds (bibtex-search-forward-field "author" t))
           (beg (bibtex-start-of-text-in-field bounds)))
      (when bounds
        (goto-char beg)
        (while (re-search-forward "\\(\\s-+\\) and"
                                  (bibtex-end-of-text-in-field bounds) t)
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

(defun km/bibtex-downcase-entry ()
  (save-excursion
    (bibtex-beginning-of-entry)
    (when (looking-at "^@\\([^{]+\\){")
      (replace-match (downcase (match-string-no-properties 1))
                     'fixedcase nil nil 1))))

(defun km/bibtex-pages-use-double-hyphen ()
  "Use double hyphen for page range."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ((bounds (bibtex-search-forward-field "pages" t)))
      (when bounds
        (goto-char (bibtex-start-of-text-in-field bounds))
        (and (re-search-forward "[^A-z0-9]*-[^A-z0-9]*"
                                (bibtex-end-of-text-in-field bounds) t)
             (replace-match "--"))))))

(defun km/bibtex-remove-doi-leader ()
  "Remove leading part (http:...) of doi URL."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ((bounds (bibtex-search-forward-field "doi" t)))
      (when bounds
        (goto-char (bibtex-start-of-text-in-field bounds))
        (and (re-search-forward "http://dx.doi.org/"
                                (bibtex-end-of-text-in-field bounds) t)
             (replace-match ""))))))

(defun km/bibtex-downcase-keys ()
  "Downcase keys that are all caps."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let (case-fold-search)
      (while (re-search-forward "^\\s-*\\([A-Z]+\\)\\s-*=" nil t)
        (replace-match (downcase (match-string 1)) 'fixedcase
                       nil nil 1)))))

(defun km/bibtex-downcase-author-and ()
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ((bounds (bibtex-search-forward-field "author" t)))
      (when bounds
        (goto-char (bibtex-start-of-text-in-field bounds))
        (let (case-fold-search)
          (while (re-search-forward "\\bAND\\b"
                                    (bibtex-end-of-text-in-field bounds) t)
            (replace-match (downcase (match-string 0)) 'fixedcase)))))))

(defvar km/bibtex-article-fields-to-delete
  '("abstract" "issn" "pubmedid" "url" "eprint" "keywords"))

(defun km/bibtex-delete-article-fields ()
  (save-excursion
    (when (and (bibtex-beginning-of-entry)
               (looking-at bibtex-entry-maybe-empty-head)
               (string= (downcase (bibtex-type-in-head)) "article"))
      (dolist (f km/bibtex-article-fields-to-delete)
       (let (bounds)
         ;; Make sure field is removed even if it is repeated.
         (while (progn (bibtex-beginning-of-entry)
                       (setq bounds (bibtex-search-forward-field f t)))
           (goto-char (bibtex-end-of-field bounds))
           (skip-chars-backward " \t\n")
           (delete-region (bibtex-start-of-field bounds)
                          (point))))))))

;;;###autoload
(defun km/browse-doi (doi)
  "Open DOI in browser.
When called interactively, take the DOI from the text under
point.  The link is opened using the settings of
`org-doi-server-url'."
  (interactive (list (km/doi-at-point)))
  (browse-url (org-link-escape-browser (concat org-doi-server-url doi))))

;;;###autoload
(defun km/copy-doi-as-kill ()
  "Copy DOI at point to kill ring."
  (interactive)
  (-when-let (doi (km/doi-at-point))
    (kill-new (message "%s" (concat "doi:" doi)))))

(defun km/doi-at-point ()
  "Return DOI at point."
  (save-excursion
    (skip-chars-backward "-.A-z0-9/")
    (and (looking-at "\\(doi:[ \t\n]*\\)*\\([-./A-z0-9]+[A-z0-9]\\)\\b")
         (match-string-no-properties 2))))

(provide 'km-bib)
;;; km-bib.el ends here