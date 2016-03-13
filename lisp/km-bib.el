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
  '("a" "an" "and" "as" "at" "but" "by" "for" "in" "nor"
    "of" "on" "or" "out" "so" "the" "to" "up" "yet")
  "Words to ignore when running `km/bibtex-use-title-case'.")

(defun km/bibtex-use-title-case ()
  "Convert title of current BibTeX entry to title case.
Change words in `km/bibtex-unimportant-title-words' to lower
case, unless the word is the first word in the title.  Capitalize
all other words unless they are protected by brackets."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ((bounds (bibtex-search-forward-field "title" t)))
      (when bounds
        (let* ((beg (1+ (bibtex-start-of-text-in-field bounds)))
               (end (1- (bibtex-end-of-text-in-field bounds)))
               (title-words (split-string
                             (buffer-substring-no-properties beg end)))
               (cap-if-letter
                (lambda (word)
                  (let ((case-fold-search nil))
                    (if (string-match-p "\\`[a-z]" word)
                        (capitalize word)
                      word))))
               (choose-case
                (lambda (word)
                  (funcall (if (member (downcase word)
                                       km/bibtex-unimportant-title-words)
                               #'downcase
                             cap-if-letter)
                           word))))
          (goto-char beg)
          (delete-region beg end)
          (insert (mapconcat
                   #'identity
                   (cons (funcall cap-if-letter (car title-words))
                         (mapcar choose-case (cdr title-words)))
                   " "))
          (fill-paragraph))))))

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
