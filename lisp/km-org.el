;;; km-org.el --- Org mode extensions

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

(require 'cl-lib)
(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-link-edit)
(require 'ox-ascii)
(require 's)

;;;###autoload
(defun km/org-tree-to-indirect-buffer (&optional arg)
  "Run `org-tree-to-indirect-buffer', keeping previous buffer.
By default, `org-tree-to-indirect-buffer' deletes the previous
indirect buffer when making a new one to avoid accumulating
buffers, which can be overriden by a C-u prefix. Reverse this
behavior so that the prefix must be given in order to delete the
previous indirect buffer. If the argument is a number, which has
a different meaning, it is left untouched."
  (interactive "P")
  (unless (numberp arg)
    (setq arg (not arg)))
  (org-tree-to-indirect-buffer arg))

;;;###autoload
(defun km/org-tree-to-indirect-buffer-current-window (&optional arg)
  "Create indirect buffer and narrow to subtree in this window.
Before running `org-tree-to-indirect-buffer', set
`org-indirect-buffer-display' to `current-window'."
  (interactive "P")
  (let ((org-indirect-buffer-display 'current-window))
    (km/org-tree-to-indirect-buffer arg)))

;;;###autoload
(defun km/org-clone-and-shift-by-repeater ()
  "Clone current subtree, shifting new timestamp by repeater.
The repeater is removed from the original subtree."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((repeater
           (and (re-search-forward
                 ;; Regexp taken from `org-clone-subtree-with-time-shift'.
                 "<[^<>\n]+ +\\([.+]?\\+[0-9]+[hdwmy]\\)"
                 (save-excursion (org-end-of-subtree)) t)
                (match-string-no-properties 1))))
      (unless repeater
        (user-error "Subtree does not have repeater"))
      (org-clone-subtree-with-time-shift 0 repeater))))

;;;###autoload
(defun km/org-delete-checked-items ()
  "Delete checked items.

If the element at point is not a plain list, search the parent
elements for a plain list, stopping when the first plain list or
headline is found.

After deleting checked items, move to the first item of the list.
If there are no items of the list remaining, move to the parent
heading."
  (interactive)
  (let* ((el (or (org-element-lineage (org-element-context) '(plain-list) t)
                 (user-error "Point is not within a plain list")))
         (beg (org-element-property :begin el))
         ;; Check maximum point because, if narrowed to a heading,
         ;; org-element can return a point beyond this.
         (end (min (org-element-property :end el) (point-max)))
         (struct (org-element-property :structure el))
         (list-level (org-list-get-ind beg struct))
         (deleted-count 0)
         (text (buffer-substring beg end))
         new-text)
    (with-temp-buffer
      (insert text)
      (let ((offset (1- beg))
            (pmax (point-max))
            level box bpos epos)
        (dolist (item (reverse struct))
          (setq level (nth 1 item)
                box (nth 4 item)
                bpos (- (nth 0 item) offset)
                ;; Minimum check here is for the same reason as
                ;; above with `end'.  This only comes into play for
                ;; the last item.
                epos (min (- (nth 6 item) offset) pmax))
          (when (and (= list-level level)
                     (string= box "[X]"))
            (delete-region bpos epos)
            (setq deleted-count (1+ deleted-count)))))
      (setq new-text (buffer-string)))
    (if (= deleted-count 0)
        (message "No checked boxes found")
      (delete-region beg end)
      (goto-char beg)
      (insert new-text)
      (goto-char beg)
      (unless (eq (car (org-element-at-point)) 'plain-list)
        (outline-previous-heading))
      (org-update-checkbox-count-maybe)
      (message "Deleted %s item(s)" deleted-count))))

(defmacro km/org--save-pos-on-sort (&rest body)
  "Try to return to the orginal position after sorting.

Sorting doesn't play well with `save-restriction' or markers, so
just put the point where it was relative to the original heading.
This may not actually be the same tree if there are redundant
headings.

This relies on point being placed at the heading that was sorted,
as `org-sort-entries' does."
  `(let ((starting-pos (point)))
     (org-back-to-heading t)
     (let ((heading-line (buffer-substring-no-properties
                          (point-at-bol) (point-at-eol)))
           (chars-after-heading (- starting-pos (point))))
       ,@body
       (search-forward heading-line)
       (beginning-of-line)
       (goto-char (+ (point) chars-after-heading)))))

;;;###autoload
(defun km/org-sort-parent (arg)
  "Sort on parent heading ARG levels up.
After sorting, return point to its previous location under the
current heading."
  (interactive "p")
  (km/org--save-pos-on-sort
   (outline-up-heading arg)
   (call-interactively #'org-sort)))

(defun km/org-sort-all-level-headings (level)
  "Sort all buffer headings that are at LEVEL (default 1)."
  (interactive "p")
  (org-map-entries (lambda ()
                     (when (and (= (org-current-level) level)
                                ;; Avoid "Nothing to sort" error.
                                (save-excursion (org-goto-first-child)))
                       (org-sort-entries nil ?a))))
  (message "Sorted headings at level %s" level))

(defun km/org--prop-sort-args ()
  "Return `org-sort-entries' arguments based on \"SORT\" property."
  (when (save-excursion (org-goto-first-child))
    (let ((prop (org-entry-get nil "sort" 'inherit)))
      (when prop
        (let* ((current-level (org-current-level))
               (sort-prop (s-split " by " prop))
               (levels (mapcar #'string-to-number (s-split nil (car sort-prop))))
               (sorting-type (cadr sort-prop))
               sorting-func)
          (if sorting-type
              (progn
                (setq sorting-type (read sorting-type))
                (cond
                 ((characterp sorting-type))
                 ((fboundp sorting-type)
                  (setq sorting-func sorting-type
                        sorting-type ?f))
                 (t
                  (user-error "Invalid sorting type: %s" sorting-type))))
            (setq sorting-type ?a))
          (when (or (equal levels (list 0))
                    (memq current-level levels))
            (list nil sorting-type sorting-func)))))))

(defun km/org-maybe-sort ()
  "Sort current heading based on \"SORT\" property.

Property value should have the format \"LEVELS by TYPE\", where
LEVELS specifies the level of heading to sort and TYPE is the
sorting type.

If LEVELS is a space-seperated list of positive integers, only
sort heading if it is at one of these levels.  If LEVELS is zero
or a non-numeric string, sort heading regardless of its level.
If LEVELS is a negative number, do not sort. (Notice that there
is only support for sorting subheadings in a tree, not top-level
headings.)

If TYPE is a character, pass it as the SORTING-TYPE argument to
`org-sort-entries'.  If TYPE is the name of a bound function,
pass it as the GETKEY-FUNC argument to `org-sort-entries' (with
?f as the SORTING-TYPE value).  If \"by TYPE\" is omitted from
the property value, sort alphabetically.

For example

  2 by ?a          Sort alphabetically if level 2 heading.
  2                Same as above.

  t                Sort heading alphabetically.
  all              Same as above.

  1 by func        Sort heading using function if level 1 heading.

  -1               Don't sort.  Useful for overriding parent value."
  (let ((sort-args (km/org--prop-sort-args)))
    (when sort-args
      (apply #'org-sort-entries sort-args))))

;;;###autoload
(defun km/org-maybe-sort-buffer-headings ()
  "Call `km/org-maybe-sort' on buffer headings."
  (interactive)
  (org-map-entries #'km/org-maybe-sort))

;;;###autoload
(defun km/org-maybe-sort-parent ()
  "Sort parent heading based on \"SORT\" property.
See `km/org-maybe-sort' for details of property value format."
  (let (heading-pos sort-args)
    (save-excursion
      (and (org-up-heading-safe)
           (setq heading-pos (point)
                 sort-args (km/org--prop-sort-args))))
    (when sort-args
      (km/org--save-pos-on-sort
       (goto-char heading-pos)
       (apply #'org-sort-entries sort-args)))))

(defun km/org-sort-heading-ignoring-articles ()
  "Sort alphabetically, but ignore any leading articles."
  (let* ((ignored-words '("a" "an" "the"))
         (heading (org-no-properties
                   (org-get-heading 'no-tags 'no-todo)))
         (heading-words (split-string heading)))
    (when (member (downcase (car heading-words))
                  ignored-words)
      (setq heading-words (cdr heading-words)))
    (mapconcat #'identity heading-words " ")))

;;;###autoload
(defun km/org-remove-title-leader ()
  "Remove leader from Org heading title.

Convert

  * TODO leader: Rest of title       :tag:

to

  * TODO Rest of title               :tag:"
  (interactive)
  (save-excursion
    (let ((regex (format "^%s\\(?:%s \\)?\\(?:%s \\)?\\(.*: \\)\\w+"
                         org-outline-regexp org-todo-regexp
                         org-priority-regexp)))
      (org-back-to-heading)
      (when (re-search-forward regex (point-at-eol) t)
        (replace-match "" nil nil nil 4)
        (org-set-tags nil t)))))

(defun km/org-add-blank-before-heading ()
  "Add a blank line before Org headings in buffer."
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (while (re-search-forward "[^\n]\n\\*" nil t)
     (when (org-at-heading-p)
       (beginning-of-line)
       (open-line 1)))))

(autoload 'km/reduce-to-single-spaces "km-editing")
;;;###autoload
(defun km/org-normalize-spaces ()
  "Reduce to single spaces and add space before headings."
  (interactive)
  (km/reduce-to-single-spaces)
  (km/org-add-blank-before-heading))

;;;###autoload
(defun km/org-switch-to-buffer-other-window (&optional arg)
  (interactive "P")
  (cl-letf (((symbol-function 'org-pop-to-buffer-same-window)
             (lambda (buffer-or-name &rest args)
               (funcall #'pop-to-buffer buffer-or-name))))
    (org-switchb arg)))

;;;###autoload
(defun km/org-open-at-point-stay ()
  "Like `org-open-at-point', but stay on heading.
This variant is convient to use in `org-speed-commands-user'
because remaining on the heading allows additional commands to be
called through the speed command interface."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not at heading"))
  (save-excursion
    (call-interactively #'org-open-at-point)))

(defun km/org-goto ()
  "Like `org-goto', but act on widened buffer.
If point ends up outside the previously narrowed region, leave
the buffer widened."
  (interactive)
  (pcase-let ((`(,beg . ,end) (and (buffer-narrowed-p)
                                   (cons (point-min) (point-max)))))
    (widen)
    (unwind-protect
        (call-interactively #'org-goto)
      (when (and beg (<= beg (point) end))
        (narrow-to-region beg end)))))

(defun km/org-grep-buffer-to-list ()
  "Convert `grep-mode' buffer to Org mode list."
  (interactive)
  (let ((result-re (rx line-start
                       (group (one-or-more (not (any ":"))))
                       ":"
                       (group (one-or-more digit))
                       ":"
                       (group (one-or-more not-newline))
                       line-end))
        (dir default-directory)
        results
        cmd)
    (save-excursion
      (goto-char (point-min))
      (forward-line 3)
      (setq cmd (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))
      (forward-line 1)
      (while (and (not (looking-at-p "\n\\s-*$"))
                  (re-search-forward result-re nil t))
        (push (list (match-string-no-properties 1)
                    (match-string-no-properties 2)
                    (match-string-no-properties 3))
              results))
      (with-current-buffer (get-buffer-create "*Org grep results*")
        (setq default-directory dir)
        (erase-buffer)
        (insert "\n* Results [/]\n\n")
        (insert (format "Call: %s\n\n" cmd))
        (pcase-dolist (`(,file ,_ ,text) (nreverse results))
          (insert (format "- [ ] %s\n"
                          (org-make-link-string
                           (concat "file:" file "::" text)
                           (let ((desc (concat file ":" text)))
                             (if (> (length desc) 72)
                                 (substring desc 0 72)
                               desc))))))
        (org-mode)
        (org-back-to-heading)
        (org-update-checkbox-count)
        (org-show-entry)
        (pop-to-buffer (current-buffer))))))


;;; Agenda


(defvar km/org-agenda-file-directory nil)

;;;###autoload
(defun km/org-agenda-cd-and-read-dir-locals ()
  (unless (get 'org-agenda-files 'org-restrict)
    (setq default-directory (expand-file-name "~/notes/"))
    (hack-local-variables)))

;;;###autoload
(defun km/org-agenda-store-current-span ()
    "Store the current span value in `org-agenda-span'.
This allows the view to persist when the agenda buffer is
killed."
    (when org-agenda-current-span
      (setq org-agenda-span org-agenda-current-span)))

;;;###autoload
(defun km/org-agenda-add-or-remove-file (file)
  "Add or remove link to FILE in `km/org-agenda-file-directory'.
If a link for FILE does not exist, create it. Otherwise, remove
it. Like `org-agenda-file-to-front', this results in FILE being
displayed in the agenda."
  (interactive (list (cl-case major-mode
                       (org-mode (buffer-file-name))
                       (dired-mode (dired-get-filename))
                       (org-agenda-mode (ignore-errors (save-window-excursion
                                                         (org-agenda-goto)
                                                         (buffer-file-name))))
                       (t (read-file-name "Link file: ")))))
  (let ((agenda-file (expand-file-name (file-name-nondirectory file)
                                       km/org-agenda-file-directory)))
    (if (file-equal-p (file-truename agenda-file) file)
        (progn
          (when (called-interactively-p) (message "Deleting %s" agenda-file))
          (delete-file agenda-file))
      (when (called-interactively-p) (message "Adding %s" agenda-file))
      (make-symbolic-link file agenda-file))))

;;;###autoload
(defun km/org-open-default-notes-file-inbox ()
  "Open \"Inbox\" heading of `org-default-notes-file'."
  (interactive)
  (find-file org-default-notes-file)
  (goto-char (org-find-exact-headline-in-buffer "Inbox" nil t))
  (recenter-top-bottom 0)
  (show-children))

;;;###autoload
(defun km/org-goto-agenda-heading ()
  "Jump to heading in agenda files."
  (interactive)
  (let ((org-refile-targets
         '((org-agenda-files :maxlevel . 3)
           (org-agenda-text-search-extra-files :maxlevel . 3)))
        (org-refile-use-outline-path t))
    (org-refile '(4))))

(defun km/org-delete-subtree ()
  (org-back-to-heading t)
  (delete-region
   (point)
   (org-element-property :end (org-element-at-point))))

(defun km/org-agenda-delete-subtree ()
  (interactive)
  (org-agenda-archive-with #'km/org-delete-subtree))

;;;###autoload
(defun km/org-agenda-set-restriction-lock (&optional type)
  "Call `org-agenda-set-restriction-lock' with flipped C-u meaning."
  (interactive "P")
  (org-agenda-set-restriction-lock
   (cond ((equal type '(4)) nil)
         (type)
         (t '(4)))))


;;; Refiling

(defvar km/org-agenda-refile-targets
  '((nil :maxlevel . 3)
    (org-agenda-files :maxlevel . 2)
    (org-agenda-text-search-extra-files :maxlevel . 2)))

(defun km/org-refile-verify-target ()
  "Exclude DONE state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defvar km/org-refile-list-item-tag "bref"
  "Tag marking heading with list that can be refiled to.")

(defun km/org-refile-list-item (&optional copy)
  "Refile list item to a heading.

Consider targets to be headings with the tag
`km/org-refile-list-item-tag' in any file listed in
`org-refile-targets'.

The item is dropped directly under the heading, after any
planning information or property drawers.  No attempt is made to
make sure that it is part of any previous list.

With prefix argument COPY, the item is not deleted from the
original list."
  (interactive "P")
  (unless (org-at-item-p)
    (user-error "Not at an item"))
  (let* ((beg (save-excursion (beginning-of-line) (point-marker)))
         (end (save-excursion
                (goto-char
                 (nth 6 (assoc (marker-position beg) (org-list-struct))))
                (point-marker)))
         (item (buffer-substring-no-properties beg end))
         (ftargets (mapcar #'car org-refile-targets))
         (org-refile-targets (mapcar
                              (lambda (f)
                                (cons f (cons :tag km/org-refile-list-item-tag)))
                              ftargets))
         (org-refile-use-outline-path t)
         (loc (org-refile-get-location "Bullet heading"))
         (fname (nth 1 loc))
         (heading-pos (nth 3 loc)))
    (with-current-buffer (or (find-buffer-visiting fname)
                             (find-file-noselect fname))
      (org-with-wide-buffer
       (goto-char heading-pos)
       (forward-line)
       (while (and (not (eobp))
                   (memq (org-element-type (org-element-at-point))
                         '(planning property-drawer node-property)))
         (forward-line))
       (insert item)
       (org-update-checkbox-count-maybe)))
    (goto-char beg)
    (unless copy
      (delete-region beg end)
      (org-update-checkbox-count-maybe))))

(defvar km/org-refile-dwim-maxlevel 2)

(defun km/org-refile-dwim ()
  "Rebind `org-refile-targets' if next window is an Org buffer.
A target is determined by `km/org-refile-dwim-target-file'."
  (interactive)
  (let* ((dwim-target (km/org-refile-dwim-target-file))
         (org-refile-targets (if dwim-target
                                 `((nil
                                    :maxlevel . ,km/org-refile-dwim-maxlevel)
                                   (dwim-target
                                    :maxlevel . ,km/org-refile-dwim-maxlevel))
                               org-refile-targets)))
    (call-interactively #'org-refile)))

(defun km/org-agenda-refile-dwim ()
  "Rebind `org-agenda-refile-targets' if next window is an Org buffer.
A target is determined by `km/org-refile-dwim-target-file'."
  (interactive)
  (let* ((dwim-target (km/org-refile-dwim-target-file))
         (org-refile-targets (if dwim-target
                                 `((nil
                                    :maxlevel . ,km/org-refile-dwim-maxlevel)
                                   (dwim-target
                                    :maxlevel . ,km/org-refile-dwim-maxlevel))
                               org-refile-targets)))
    (call-interactively #'org-agenda-refile)))

(defun km/org-refile-dwim-target-file ()
  "Return next window that is an Org buffer."
  (let* ((from-buffer (current-buffer))
         (other-win (get-window-with-predicate
                     (lambda (w)
                       (with-current-buffer (window-buffer w)
                         (and (derived-mode-p 'org-mode)
                              (not (eq from-buffer (current-buffer)))))))))
    (and other-win
         (buffer-file-name (window-buffer other-win)))))

(defun km/org-refile-to-other-file (file &optional maxlevel)
  "Refile with `org-refile-targets' set to FILE.
A numeric prefix sets MAXLEVEL (defaults to 2)."
  (interactive "fFile: \nP")
  (let* ((maxlevel (prefix-numeric-value (or maxlevel 2)))
         (file (substring-no-properties file))
         (org-refile-targets `((,file :maxlevel . ,maxlevel))))
    (org-refile)))

(defun km/org-refile-to-other-org-buffer (buffer &optional maxlevel)
  "Refile with `org-refile-targets' set to BUFFER file name.
A numeric prefix sets MAXLEVEL (defaults to 2)."
  (interactive (list (km/get-org-file-buffer) current-prefix-arg))
  (km/org-refile-to-other-file (buffer-file-name buffer)
                               maxlevel))

(defun km/get-org-file-buffer ()
  (get-buffer
   (org-icompleting-read "Buffer: " (mapcar 'buffer-name
                                            (org-buffer-list 'files)))))

(defun km/org-set-refiling-buffer (&optional maxlevel)
  "Choose buffer to set as sole target in `org-refile-targets'.
If `org-refile-targets' is already a local variable, restore the
global value. A numeric prefix sets MAXLEVEL (defaults to 2)."
  (interactive "P")
  (if (local-variable-p 'org-refile-targets)
      (kill-local-variable 'org-refile-targets)
    (let ((buffer-file (substring-no-properties
                        (buffer-file-name (km/get-org-file-buffer))))
          (maxlevel (prefix-numeric-value (or maxlevel 2))))
      (set (make-local-variable 'org-refile-targets)
           `((,buffer-file :maxlevel . ,maxlevel))))))


;;; Links

(defvar km/org-pmid-search-url "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s"
  "URL to search for PMID.")

;;;###autoload
(defun km/org-pmid-open (path)
  "Search for PMID at `km/org-pmid-search-url'."
  (browse-url (format km/org-pmid-search-url path)))

(defun km/org-link-dired-jump ()
  "Open Dired for directory of file link at point."
  (interactive)
  (let ((el (org-element-lineage (org-element-context) '(link) t)))
    (unless (and el (equal (org-element-property :type el) "file"))
      (user-error "Not on file link"))
    (dired-jump 'other-window
                (expand-file-name (org-element-property :path el)))))

(defun km/org-link-edit-slurp-link ()
  "Slurp trailing text into link.

  \[link\]extra  ->  \[\[linkextra\]\]

After slurping, return the slurped text and move point to the
beginning of the link."
  (interactive)
  (cl-multiple-value-bind (beg end link desc) (org-link-edit--get-link-data)
    (when (progn (goto-char end) (looking-at "[^ \t\n]+"))
      (let ((slurped (match-string-no-properties 0)))
        (setq link (concat link slurped)
              end (match-end 0))
        (delete-region beg end)
        (insert (org-make-link-string link desc))
        (goto-char beg)
        slurped))))


;;; Export

(defvar km/org-md-fill-column fill-column
  "Fill column for exported markdown.
This is a separate variable instead of `fill-column' to allow it
to be easily overriden.")


;;;###autoload
(defun km/org-md-fill-string (contents)
  "Use `org-ascii--fill-string' to fill ox-md paragraphs."
  (org-ascii--fill-string contents km/org-md-fill-column
                          nil))

(defun km/org-md-export-unfilled-buffer ()
  (interactive)
  (let ((km/org-md-fill-column (point-max)))
    (org-md-export-as-markdown)))


;;; Org open file

;;;###autoload
(defun km/org-open-file-at-point ()
  "Open file at point with `org-open-file'."
  (interactive)
  (if (and (derived-mode-p 'org-mode)
           (org-element-lineage (org-element-context) '(link) t))
      (org-open-at-point)
    (let ((file (or (and (use-region-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end)))
                    (thing-at-point 'filename))))
      (if (and file (file-exists-p file))
          (org-open-file file)
        (km/org-open-file)))))

;;;###autoload
(defun km/org-open-file ()
  "Interactive version of `org-open-file'."
  (interactive)
  (org-open-file (read-file-name "Open file: " nil nil t)))

;;;###autoload
(defun km/org-open-last-pdf ()
  (interactive)
  (let ((file (cl-find-if (lambda (x) (string-match-p "\\.pdf\\'" x))
                          recentf-list)))
    (when file
      (org-open-file file))))

(autoload 'magit-annex-present-files "magit-annex")
(autoload 'magit-completing-read "magit-utils")
;;;###autoload
(defun km/org-open-annex-file ()
  "Open a git annex file with `org-open-file'."
  (interactive)
  (--if-let (magit-annex-present-files)
      (org-open-file (magit-completing-read "Open annex file" it nil t))
    (message "No annex files found")))

(autoload 'km/read-recent-file "km-files")
;;;###autoload
(defun km/org-open-recent-file ()
  "Open a file from `recentf-list' with `org-open-file'."
  (interactive)
  (org-open-file (km/read-recent-file)))

(provide 'km-org)
;;; km-org.el ends here
