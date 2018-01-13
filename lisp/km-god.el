;;; km-god.el --- God mode extensions

;; Copyright (C) 2012-2018 Kyle Meyer <kyle@kyleam.com>

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

(require 'god-mode)

(defun km/god-update-cursor ()
  (setq cursor-type (if god-local-mode 'bar 'box)))

(defun km/god-gnus-p ()
  "Return non-nil if a Gnus-related mode is enabled."
  (derived-mode-p 'gnus-group-mode
                  'gnus-summary-mode
                  'gnus-article-mode
                  'message-mode))

(provide 'km-god)
;;; km-god.el ends here
