(setq org-completion-use-ido t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(org-babel-do-load-languages 'org-babel-load-languages  '((scheme . t)))

(when (file-directory-p "~/Dropbox/doc/org/")
  (setq org-directory "~/Dropbox/doc/org/"
        org-default-notes-file (concat org-directory "notes.org")
        org-agenda-files (concat org-directory "todo.org")
        org-capture-templates '(("t" "Todo" entry (file+headline
                                                   (concat org-directory "todo.org")
                                                   "Tasks")
                                 "* TODO %?\n  %i\n  %a")
                                ("n" "Notes" entry (file org-default-notes-file)
                                 "* %?\n  %i\n  %a"))))
