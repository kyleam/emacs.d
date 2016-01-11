;;; setkey.el --- Set a temporary key binding

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

(defvar setkey-command nil)
(defvar setkey-last-call-time nil)
(defvar setkey-seconds-timeout 600)

;;;###autoload
(defun setkey-call ()
  "Call `setkey-command'.
When `setkey-command' is nil or the time since the last call
has exceeded `setkey-seconds-timeout', read the command to
call."
  (interactive)
  (when (or (not setkey-command)
            (> (- (float-time) setkey-last-call-time)
               setkey-seconds-timeout))
    (setq setkey-command (read-command "Command: " setkey-command)))
  (setq setkey-last-call-time (float-time))
  (call-interactively setkey-command))

(defun setkey-reset ()
  "Reset `setkey-call' command."
  (interactive)
  (setq setkey-command nil
        setkey-last-call-time nil))

(provide 'setkey)
;;; setkey.el ends here
