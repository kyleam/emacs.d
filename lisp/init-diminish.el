(require 'diminish)

(diminish 'abbrev-mode "Ab")
(after 'flyspell (diminish 'flyspell-mode "Fy"))
(after 'paredit (diminish 'paredit-mode " Pe"))
(after 'magit (diminish 'magit-auto-revert-mode))
(after 'mml (diminish 'mml-mode "Ml"))
(after 'org (diminish 'orgstruct-mode "Os"))
(after 'org-table (diminish 'orgtbl-mode "Ot"))
(after 'projectile (diminish 'projectile-mode))
(after 'reftex (diminish 'reftex-mode "Rf"))
(after 'view (diminish 'view-mode "Vw"))

(provide 'init-diminish)
