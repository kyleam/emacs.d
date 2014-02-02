(add-to-list 'load-path "~/src/emacs/org-mode/contrib/lisp" t)

(autoload 'org-contacts-template-name "org-contacts"
  "Try to return the contact name for a template.
If not found return RETURN-VALUE or something that would ask the user.")

(setq org-contacts-files '("~/notes/contacts.org"))

(add-to-list 'org-capture-templates
             '("a" "email address" entry (file+headline "~/notes/contacts.org" "Inbox")
               "** %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:"))

(provide 'init-orgcontacts)
