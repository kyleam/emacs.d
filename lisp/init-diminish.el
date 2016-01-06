;;; init-diminish.el --- Diminish mode configuration

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

(require 'diminish)

(diminish 'abbrev-mode "Ab")
(after 'helm-mode (diminish 'helm-mode))
(after 'flyspell (diminish 'flyspell-mode "Fy"))
(after 'paredit (diminish 'paredit-mode " Pe"))
(after 'mml (diminish 'mml-mode "Ml"))
(after 'org (diminish 'orgstruct-mode "Os"))
(after 'org-table (diminish 'orgtbl-mode "Ot"))
(after 'projectile (diminish 'projectile-mode))
(after 'reftex (diminish 'reftex-mode "Rf"))
(after 'view (diminish 'view-mode "Vw"))
(after 'whitespace (diminish 'global-whitespace-mode))
(after 'yasnippet (diminish 'yas-minor-mode))

(provide 'init-diminish)
;;; init-diminish.el ends here
