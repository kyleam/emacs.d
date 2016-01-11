;;; km-webjump.el --- Extensions for webjump

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
(require 'webjump)

(defun km/webjump-read-string (prompt)
  "Like `webjump-read-string', but set default."
  (let* ((default (if (use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (thing-at-point 'symbol)))
         (prompt (if default
                     (format "%s (%s): " prompt default)
                   (concat prompt ": ")))
         (input (read-string prompt nil nil default)))
    (unless (webjump-null-or-blank-string-p input)
      (substring-no-properties input))))

;;;###autoload
(defun km/webjump ()
  "Run`webjump' with symbol at point or region as default query.
This affects only sites in the `simple-query' format."
  (interactive)
  (cl-letf (((symbol-function 'webjump-read-string) #'km/webjump-read-string))
    (call-interactively #'webjump)))

(provide 'km-webjump)
;;; km-webjump.el ends here
