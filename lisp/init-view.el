;;; init-view.el --- View mode configuration

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

(after 'view
  (define-key view-mode-map "l" 'recenter-top-bottom)
  (define-key view-mode-map "f" 'forward-word)
  (define-key view-mode-map "b" 'backward-word)
  (define-key view-mode-map "]" 'forward-paragraph)
  (define-key view-mode-map "[" 'backward-paragraph)
  (define-key view-mode-map "j" 'avy-goto-subword-1))

(key-chord-define-global "hq" 'view-mode)

(provide 'init-view)
