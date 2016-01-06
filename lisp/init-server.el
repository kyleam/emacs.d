;;; init-server.el --- Emacs server configuration

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

(setq server-use-tcp t)
(require 'server)
(unless (server-running-p)
  (server-start))

(let ((server (daemonp)))
  (cond
   ((string= server "default")
    ;; Remove all mail map bindings except notmuch.
    (global-set-key (kbd "C-x m") nil)
    (global-set-key (kbd "C-x m n") 'notmuch)
    (add-hook 'kill-emacs-hook #'km/pydoc-save-names-file)
    (setq save-abbrevs 'silently
          bookmark-save-flag 1))
   ((string= server "mail")
    (setq mode-line-misc-info
          (cons (propertize " [Mail] " 'face 'font-lock-doc-face)
                mode-line-misc-info))
    (key-chord-define-global "jg" 'km/mail-map)
    (setq recentf-save-file "~/.emacs.d/cache/recentf-mail")
    (setq save-abbrevs nil))))

(provide 'init-server)
;;; init-server.el ends here
