;; these could be kept alongside related topics, but for whatever reason
;; I prefer having them together

(defvar km/modes '(("\\.zsh$" . shell-script-mode)
                   ("\\.*rc$" . conf-unix-mode)
                   ("\\.org.txt$" . org-mode)
                   ("PKGBUILD" . pkgbuild-mode))
  "Auto mode mappings")

(mapc
 (lambda (mode) (setq auto-mode-alist
                      (cons mode auto-mode-alist)))
 km/modes)
