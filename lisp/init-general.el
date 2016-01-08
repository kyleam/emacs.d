;;; init-general.el --- Things without a more specific home

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

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t
      enable-recursive-minibuffers t)

(setq tramp-default-method "sshx")

(setq-default indicate-empty-lines t
              indent-tabs-mode nil)

(setq set-mark-command-repeat-pop t)

(setq recenter-positions '(top middle bottom))

;; This is intentionally not loaded.
(setq custom-file "~/.emacs.d/.custom.el")

(setq bookmark-save-flag nil)

(setq default-input-method "TeX")

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.*rc\\'" . conf-unix-mode))

(global-set-key (kbd "C-h ;") 'find-function)
(global-set-key (kbd "C-h 4 ;") 'find-function-other-window)


(global-set-key (kbd "C-c l") 'helm-imenu)

;; Disable `suspend-frame' binding.
(global-set-key (kbd "C-x C-z") nil)

;; Avoid shift key for `backward-paragraph' and `forward-paragraph'.
(global-set-key (kbd "M-}") nil)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-{") nil)
(global-set-key (kbd "M-[") 'backward-paragraph)

(global-set-key (kbd "C-c x") 'eval-expression)

(show-paren-mode)
(global-auto-revert-mode)
(key-chord-mode 1)


;;; Set key

(defvar km/setkey-command nil)
(defvar km/setkey-last-call-time nil)
(defvar km/setkey-seconds-timeout 600)

(defun km/setkey-call ()
  "Call `km/setkey-command'.
When `km/setkey-command' is nil or the time since the last call
has exceeded `km/setkey-seconds-timeout', read the command to
call."
  (interactive)
  (when (or (not km/setkey-command)
            (> (- (float-time) km/setkey-last-call-time)
               km/setkey-seconds-timeout))
    (setq km/setkey-command (read-command "Command: " km/setkey-command)))
  (setq km/setkey-last-call-time (float-time))
  (call-interactively km/setkey-command))

(defun km/setkey-reset ()
  "Reset `km/setkey-call' command."
  (interactive)
  (setq km/setkey-command nil
        km/setkey-last-call-time nil))

(global-set-key (kbd "C-c v") 'km/setkey-call)

(provide 'init-general)
;;; init-general.el ends here
