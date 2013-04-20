;; these could be kept alongside related topics, but for whatever reason
;; I prefer having them together

(defvar km/modes '(
                   ("\\.md$" . markdown-mode)
                   ("\\.markdown$" . markdown-mode)
                   ("\\.zsh$" . shell-script-mode)
                   ("\\.*rc$" . conf-unix-mode)
                   ("\\.org.txt$" . org-mode)
                   ("/mutt" . mail-mode)
                   )
  "Auto mode mappings")

(mapcar
 (lambda (mode) (setq auto-mode-alist
                      (cons mode auto-mode-alist)))
 km/modes)
