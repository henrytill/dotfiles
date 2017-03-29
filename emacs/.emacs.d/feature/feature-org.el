(defun ht/turn-on-org-bullets-mode ()
  (when (window-system)
    (org-bullets-mode 1)))

(defun ht/prettify-org-mode ()
  (interactive)
  (let ((variable-faces '((org-agenda-structure . 2.0)
                          (org-document-title   . 1.5)
                          (org-level-1          . 1.75)
                          (org-level-2          . 1.5)
                          (org-level-3          . 1.25)
                          (org-level-4          . 1.1)
                          (org-level-5          . nil)
                          (org-level-6          . nil)
                          (org-level-7          . nil)
                          (org-level-8          . nil)))
        (fixed-faces     '(org-agenda-date
                           org-agenda-date-today
                           org-block
                           org-code
                           org-date
                           org-footnote
                           org-link
                           org-table
                           org-verbatim))
        (meta-faces      '(org-block-begin-line
                           org-block-end-line
                           org-document-info-keyword
                           org-meta-line)))
    (dolist (face variable-faces)
      (if (cdr face)
          (set-face-attribute (car face) nil :height (cdr face) :inherit 'variable-pitch)
        (set-face-attribute (car face) nil :inherit 'variable-pitch)))
    (dolist (face fixed-faces)
      (set-face-attribute face nil :inherit 'fixed-pitch))
    (dolist (face meta-faces)
      (set-face-attribute face nil :inherit 'fixed-pitch :foreground (face-foreground 'font-lock-comment-face nil)))
    (ht/comment
      ;; At some point this was necessary for proper formatting
      (setq org-src-fontify-natively nil))
    (variable-pitch-mode t)))

(use-package org
  :ensure org-plus-contrib
  :bind (("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :functions org-bookmark-jump-unhide
  :init
  (use-package org-bullets :ensure t :commands ht/turn-on-org-bullets-mode)
  (add-hook 'org-mode-hook #'ht/prettify-org-mode)
  (add-hook 'org-mode-hook #'ht/turn-on-org-bullets-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((clojure    . t)
                                                           (emacs-lisp . t)
                                                           (forth      . t)
                                                           (haskell    . t)
                                                           (js         . t)
                                                           (ocaml      . t)
                                                           (oz         . t)
                                                           (scala      . t)
                                                           (scheme     . t)
                                                           (shell      . t)))
  (with-eval-after-load 'ob-scheme
    (load-file (locate-file "ob-scheme-fix.el" load-path)))
  (setq org-babel-clojure-backend 'cider
        org-clock-persist 'history
        org-completion-use-ido t
        org-confirm-babel-evaluate nil
        org-directory "~/org"
        org-edit-src-content-indentation 0
        org-export-babel-evaluate nil
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window)
  (setq org-link-abbrev-alist
        '(("hoogle"         . "https://www.haskell.org/hoogle/?hoogle=")
          ("pinboard-topic" . "https://pinboard.in/u:henrytill/t:")))
  (org-clock-persistence-insinuate)
  (when (file-directory-p org-directory)
    (let* ((agenda-dir (expand-directory-name "agenda" org-directory))
           (notes-file (expand-file-name "notes.org" org-directory))
           (todo-file  (expand-file-name "todo.org"  org-directory))
           (notes-template
            `("n" "Notes" entry (file ,notes-file) "* %?\n  %i\n  %a"))
           (todo-template
            `("t" "Todo" entry (file+headline ,todo-file "Tasks") "* TODO %?\n  %i\n  %a")))
      (setq org-agenda-files (cons agenda-dir (ht/list-subdirs agenda-dir '(".git")))
            org-capture-templates (list notes-template todo-template)
            org-default-notes-file notes-file)
      (set-register ?n `(file . ,(expand-file-name "notes.org" org-directory))))))

(provide 'feature-org)
