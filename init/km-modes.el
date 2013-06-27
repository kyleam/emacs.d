;; these could be kept alongside related topics, but for whatever reason
;; I prefer having them together

(defvar km/modes '(
                   ("\\.zsh$" . shell-script-mode)
                   ("\\.*rc$" . conf-unix-mode)
                   ("\\.org.txt$" . org-mode)
                   ("/mutt" . mail-mode)
                   ("PKGBUILD" . shell-script-mode)
                   )
  "Auto mode mappings")

(mapcar
 (lambda (mode) (setq auto-mode-alist
                      (cons mode auto-mode-alist)))
 km/modes)
