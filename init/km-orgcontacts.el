(require 'org-contacts)

(setq org-contacts-files '("~/notes/contacts.org"))

(add-to-list 'org-capture-templates
             '("a" "email address" entry (file "~/notes/contacts.org")
               "** %(org-contacts-template-name)\n   :PROPERTIES:\n   :EMAIL: %(org-contacts-template-email)\n   :END:"))
