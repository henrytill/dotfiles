;; Org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)

(when (file-directory-p "~/Dropbox/doc/org/")
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda))

(when (and (is-darwin-p) (window-system))
  (global-set-key (kbd "C-z") 'ns-do-hide-emacs))
