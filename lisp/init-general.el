
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t
      enable-recursive-minibuffers t)

(setq tramp-default-method "ssh")

(setq-default indicate-empty-lines t
              indent-tabs-mode nil)

(setq set-mark-command-repeat-pop t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq save-abbrevs 'silently)

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(defun km/imenu (rescan)
  "Call `imenu', rescanning if RESCAN is non-nil."
  (interactive "P")
  (when (and rescan
             ;; No need to rescan if imenu hasn't been autoloaded yet.
             (fboundp 'imenu--cleanup))
    ;; Taken from `imenu-choose-buffer-index'.
    (imenu--cleanup)
    (setq imenu--index-alist nil))
  (call-interactively #'imenu))

;; Taken from
;; http://milkbox.net/note/single-file-master-emacs-configuration/.
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun km/abbrev-add-case-global ()
  "Define lower abbreviation for the word before point.
Like `add-global-abbrev', but always make the abbreviation the
lower case variant of the word before point."
  (interactive)
  ;; Modified from `add-abbrev'.
  (let* ((table global-abbrev-table)
         (exp (buffer-substring-no-properties
               (point)
               (save-excursion (forward-word -1) (point))))
         (name (downcase exp)))
    (when (or (not (abbrev-expansion name table))
              (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                                name (abbrev-expansion name table))))
      (define-abbrev table name exp))))

(defun km/abbrev-inverse-add-uppercase-global ()
  "Define uppercase expansion for the word before point.
Like `inverse-add-global-abbrev', but always use the lower case
version of the word before point as the abbreviation and the
upper case version as the expansion."
  (interactive)
  ;; Modified from `inverse-add-abbrev'.
  (let* ((table global-abbrev-table)
         (end (point))
         (start (save-excursion (forward-word -1) (point)))
         (name (downcase (buffer-substring-no-properties start end)))
         (exp (upcase name)))
    (when (or (not (abbrev-expansion name table))
              (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                                name (abbrev-expansion name table))))
      (define-abbrev table name exp)
      (save-excursion
        (goto-char end)
        (expand-abbrev)))))

(global-set-key (kbd "C-h ;") 'find-function)

(global-set-key (kbd "C-c l") 'km/imenu)

;; Disable `suspend-frame' binding.
(global-set-key (kbd "C-x C-z") nil)

;; Avoid shift key for `backward-paragraph' and `forward-paragraph'.
(global-set-key (kbd "M-}") nil)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-{") nil)
(global-set-key (kbd "M-[") 'backward-paragraph)

(define-key occur-mode-map "n" 'next-line)
(define-key occur-mode-map "p" 'previous-line)

(define-key abbrev-map "c" 'km/abbrev-add-case-global)
(define-key abbrev-map "iu" 'km/abbrev-inverse-add-uppercase-global)

(show-paren-mode)
(global-auto-revert-mode)
(transient-mark-mode -1)
(key-chord-mode 1)

(provide 'init-general)
