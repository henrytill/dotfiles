;;;; Org

(setq org-completion-use-ido t
      org-confirm-babel-evaluate nil
      org-src-fontify-natively t)

(when (file-directory-p org-directory)
  (let* ((notes-file
          (expand-file-name "notes.org" org-directory))
         (notes-template
          `("n" "Notes" entry (file ,notes-file) "* %?\n  %i\n  %a"))
         (todo-file
          (expand-file-name "todo.org" org-directory))
         (todo-template
          `("t" "Todo" entry (file+headline ,todo-file "Tasks") "* TODO %?\n  %i\n  %a")))
    (setq org-agenda-files (list org-directory)
          org-capture-templates (list notes-template
                                      todo-template)
          org-default-notes-file notes-file)))

(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                         (scheme . t)))
