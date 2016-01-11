;;; km-ess.el --- ESS extensions

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

(require 'ess-comp)
(require 'ess-inf)
(require 'ess-s-l)

;;;###autoload
(defun km/ess-eval-buffer-up-to-line ()
  "Send up to the current line to inferior ESS process."
  (interactive)
  (ess-eval-region (point-min) (line-end-position) nil))

(defvar km/ess-dplry-pipe-key "|")

;;;###autoload
(defun km/ess-insert-dplyr-pipe ()
  "Insert `km/ess-dplry-pipe' using `ess-smart-S-assign'.
Based on instructions in `ess-smart-S-assign-key', I didn't think
this would work, but it seems to so far."
  (interactive)
  (let ((ess-S-assign " %>% ")
        (ess-smart-S-assign-key km/ess-dplry-pipe-key))
    (call-interactively #'ess-smart-S-assign)))

(provide 'km-ess)
;;; km-ess.el ends here
