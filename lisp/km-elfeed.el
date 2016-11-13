;;; km-elfeed.el --- Extensions for elfeed

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

(defvar km/elfeed-tag-keys nil
  "Keys used by `km/elfeed-set-tag' to select tags.
? and . are reserved.")

;;;###autoload
(defun km/elfeed-set-tag ()
  "Filter to a tag defined in `km/elfeed-tag-keys'.
? and . are reserved for showing a help buffer and resetting the
filter to the default, respectively."
  (interactive)
  (let ((tag-key (read-char-choice
                  (concat "Tag [?. "
                          (mapconcat (lambda (x) (char-to-string (car x)))
                                     km/elfeed-tag-keys "")
                          "]: ")
                  (append (list ?. ??)
                          (mapcar #'car km/elfeed-tag-keys)))))
    (if (eq tag-key ??)
        (with-current-buffer (get-buffer-create "*Elfeed tags*")
          (erase-buffer)
          (insert (format "\n.  default filter\n\n"))
          (insert (mapconcat (lambda (x) (format "%c  %s" (car x) (cdr x)))
                             km/elfeed-tag-keys
                             "\n"))
          (display-buffer (current-buffer)))
      (elfeed-search-set-filter
       (concat (default-value 'elfeed-search-filter)
               (and (not (eq tag-key ?.))
                    (format " +%s"
                            (cdr (assq tag-key km/elfeed-tag-keys)))))))))

;;;###autoload
(defun km/elfeed-default-filter ()
  (interactive)
  (elfeed-search-set-filter (default-value 'elfeed-search-filter)))

;;;###autoload
(defun km/elfeed-catchup ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread)
  (km/elfeed-default-filter))

(provide 'km-elfeed)
;;; km-elfeed.el ends here
