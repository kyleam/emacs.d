(add-to-list 'load-path "~/src/emacs/org-mode/contrib/lisp" t)
(require 'org-contacts)

(setq org-contacts-files '("~/notes/contacts.org"))

(add-to-list 'org-capture-templates
             '("a" "email address" entry (file+headline "~/notes/contacts.org" "Inbox")
               "** %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:"))

(provide 'init-orgcontacts)
