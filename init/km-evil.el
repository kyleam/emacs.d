;; evil mode uses undo-tree
(require 'undo-tree)

(require 'evil)
(evil-mode 1)
;; much of this it taken from http://permalink.gmane.org/gmane.emacs.vim-emulation/1135

;; red box in emacs mode
(setq evil-emacs-state-cursor '("red" box))

(define-key evil-motion-state-map "H" 'evil-scroll-up)
(define-key evil-motion-state-map "L" 'evil-scroll-down)

(defun km/save-and-kill-buffer ()
  "Save current buffer and then kill it"
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  )


(define-key evil-normal-state-map ",w" 'save-buffer)
(define-key evil-normal-state-map ",q" 'kill-buffer)
(define-key evil-normal-state-map ",d" 'km/save-and-kill-buffer)
(define-key evil-normal-state-map "Q" 'fill-paragraph)

(evil-define-key 'visual emacs-lisp-mode-map
  ",e" 'eval-region)
(evil-define-key 'visual lisp-interaction-mode-map
  ",e" 'eval-region)

(evil-define-key 'normal mail-mode-map ",q" 'server-edit)
(evil-define-key 'normal mail-mode-map ",d" 'server-edit)

(define-key evil-visual-state-map ",c" 'comment-or-uncomment-region)
(fset 'comment-this-line ;; this depends on evil bindings
      "0v$,c")
(define-key evil-normal-state-map ",c" 'comment-this-line)

(require 'key-chord)
(key-chord-mode 1)

(key-chord-define-global ",r" 'km/recentf-ido-find-file)
(key-chord-define-global ",t" 'org-capture)
;; instead of alt-x
(key-chord-define-global ",x" 'execute-extended-command)
(key-chord-define-global ",f" 'find-file)
(key-chord-define-global ",g" 'indent-relative)
(key-chord-define-global ",b" 'ido-switch-buffer)
;; cannot map this to insert mode because it jams up the first letter
;; switch from jf because often pressing j to go up/down, so probably
;; best not to have it have to consider whether it is a chord each time
(key-chord-define-global ";a" 'evil-normal-state)

;; org bindings
(defun km/always-insert-item ()
  (interactive)
  (if (not (org-in-item-p))
      (insert "\n- ")
    (org-insert-item)))

;; most from cofi config
(evil-define-key 'normal org-mode-map
  (kbd "RET") 'org-open-at-point
  "za"        'org-cycle
  "zA"        'org-shifttab
  "zm"        'hide-body
  "zr"        'show-all
  "zo"        'show-subtree
  "zO"        'show-all
  "zc"        'hide-subtree
  "zC"        'hide-all
  ",e"        'org-export-as-pdf
  ",a"        'org-archive-subtree
  ",s"        'org-todo
  "O"          (lambda ()
                 (interactive)
                 (end-of-line)
                 (org-insert-heading t)
                 (evil-append nil))
  "o"           (lambda ()
                 (interactive)
                 (end-of-line)
                 (km/always-insert-item)
                 (evil-append nil))
(kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'normal orgstruct-mode-map
  (kbd "RET") 'org-open-at-point
  "za"        'org-cycle
  "zA"        'org-shifttab
  "zm"        'hide-body
  "zr"        'show-all
  "zo"        'show-subtree
  "zO"        'show-all
  "zc"        'hide-subtree
  "zC"        'hide-all
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'insert org-mode-map
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'insert orgstruct-mode-map
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)
