;;; init-bog.el --- Bog mode configuration

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

(add-to-list 'load-path "~/src/emacs/bog/")
(require 'bog-autoloads)

(setq bog-subdirectory-group 2
      bog-combined-bib-ignore-not-found t
      bog-use-citekey-cache t)

(setq bog-keymap-prefix (kbd "C-c b"))

(add-hook 'org-mode-hook 'bog-mode)

(global-set-key bog-keymap-prefix bog-command-map)

(provide 'init-bog)
;;; init-bog.el ends here
