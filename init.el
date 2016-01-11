;;; init.el --- Kyle Meyer's Emacs configuration

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

;; (package-initialize)

(defvar km/init-lisp-dir (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path km/init-lisp-dir)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'cask)
(cask-initialize)
(require 'pallet)
(pallet-mode 1)

(require 'init-appearance)

(require 'cl-lib)
(require 'dash)
(require 's)

(require 'init-util)
(require 'init-general)
(require 'init-abbrev)
(require 'init-diminish)

(require 'init-org)
(require 'init-helm)

(require 'init-files)
(require 'init-buffers)
(require 'init-framewin)
(require 'init-ace)
(require 'init-view)

(require 'init-editing)
(require 'init-outline)
(require 'init-god)

(require 'init-elisp)
(require 'init-haskell)
(require 'init-python)
(require 'init-ess)

(require 'init-tex)
(require 'init-bib)
(require 'init-bog)

(require 'init-dired)
(require 'init-git)
(require 'init-projectile)
(require 'init-snakemake)

(require 'init-external)

(require 'init-yas)

(when (file-exists-p (expand-file-name "init-untracked.el" km/init-lisp-dir))
  (require 'init-untracked))

(require 'init-mail)
(require 'init-server)

;;; init.el ends here
