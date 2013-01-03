;; ev.org - evil mode configuration

;; evil mode uses undo-tree
(require 'undo-tree)

(require 'evil)
(evil-mode 1)
;; much of this it taken from http://permalink.gmane.org/gmane.emacs.vim-emulation/1135

;; red box in emacs mode
(setq evil-emacs-state-cursor '("red" box))

(define-key evil-motion-state-map "H" 'evil-scroll-up)
(define-key evil-motion-state-map "L" 'evil-scroll-down)

(defun save-and-kill-buffer ()
  "Save current buffer and then kill it"
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  )

(defun my-back-one-window ()
  "Go back one windo"
  (interactive)
  (other-window -1))

(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-normal-state-map "Q" 'fill-paragraph)
(define-key evil-normal-state-map ",b" 'ido-switch-buffer)
(define-key evil-normal-state-map ",d" 'save-and-kill-buffer)
(define-key evil-visual-state-map ",c" 'comment-or-uncomment-region)

(define-key evil-normal-state-map ",j" 'other-window)
(define-key evil-normal-state-map ",k" 'my-back-one-window)
(define-key evil-normal-state-map ",q" 'kill-buffer)
(define-key evil-normal-state-map ",a" 'org-archive-subtree)

(fset 'comment-this-line ;; this depends on evil bindings
      "0v$,c")
(define-key evil-normal-state-map ",c" 'comment-this-line)

;; for simult keys
;; evil mode uses undo-tree
;; again, adopted from here: http://permalink.gmane.org/gmane.emacs.vim-emulation/1135
(add-to-list 'load-path "~/.emacs-packages/key-chord")
(require 'key-chord)
(key-chord-mode 1)

(key-chord-define-global "jf" 'evil-normal-state)
(key-chord-define-global ",r" 'recentf-ido-find-file)
(key-chord-define-global ",t" 'org-capture)
(key-chord-define-global ",e" 'org-export-as-pdf)
;; instead of alt-x
(key-chord-define-global ",x" 'execute-extended-command)
(key-chord-define-global ",f" 'find-file)
