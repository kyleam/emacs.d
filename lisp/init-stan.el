(require-package 'stan-mode)
(require 'stan-mode)

;; Remove the anonymous function that activates flymake. This is not a
;; good solution because it will likely break when this hook is
;; modified, but I don't see a stan-mode variable to disable flymake.
(remove-hook 'stan-mode-hook (car (last stan-mode-hook)))

(provide 'init-stan)
