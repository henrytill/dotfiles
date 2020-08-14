(use-package ace-window          :ensure t :defer t)
(use-package dash                :ensure t :defer t)
(use-package idle-highlight-mode :ensure t :defer t)
(use-package pkg-info            :ensure t :defer t)
(use-package queue               :ensure t :defer t)
(use-package spinner             :ensure t :defer t)
(use-package docker-tramp        :ensure t :defer t)

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode)

(use-package alert
  :ensure t
  :defer t
  :config
  (cond
   ((is-darwin-p) (setq-default alert-default-style 'ignore))
   ((is-linux-p)  (setq-default alert-default-style 'libnotify))))

(use-package auth-source
  :defer t
  :config
  (let ((authinfo (expand-file-name "authinfo.gpg" "~/Dropbox/doc")))
    (when (file-exists-p authinfo)
      (add-to-list 'auth-sources authinfo))))

(use-package exec-path-from-shell
  :if (or (is-darwin-p) (is-linux-p))
  :ensure t
  :commands exec-path-from-shell-copy-env
  :init
  (setq exec-path-from-shell-check-startup-files nil))

(use-package grep
  :commands (grep find-grep find-grep-dired)
  :config
  (add-to-list 'grep-find-ignored-directories "target"))

(use-package hideshow
  :bind (("<f5>"     . ht/hs-toggle-hiding)
         ("M-<f5>"   . ht/hs-hide-all)
         ("M-S-<f5>" . ht/hs-show-all))
  :functions ht/hs-add-xml-mode
  :preface
  (defun ht/hs-minor-mode ()
    (when (not (bound-and-true-p hs-minor-mode))
      (hs-minor-mode)))
  (defun ht/hs-toggle-hiding ()
    (interactive)
    (ht/hs-minor-mode)
    (hs-toggle-hiding))
  (defun ht/hs-hide-all ()
    (interactive)
    (ht/hs-minor-mode)
    (hs-hide-all))
  (defun ht/hs-show-all ()
    (interactive)
    (ht/hs-minor-mode)
    (hs-show-all))
  :config
  (defun ht/hs-add-xml-mode (mode forward-sexp-func)
    (let ((start         "<!--\\|<[^/>]*[^/]>")
          (end           "-->\\|</[^/>]*[^/]>")
          (comment-start "<!--"))
      (push (list mode start end comment-start forward-sexp-func nil)
            hs-special-modes-alist)))
  (ht/hs-add-xml-mode 'nxml-mode 'nxml-forward-element)
  (ht/hs-add-xml-mode 'html-mode 'sgml-skip-tag-forward)
  (ht/hs-add-xml-mode 'sgml-mode 'sgml-skip-tag-forward))

(use-package hl-line-mode
  :commands hl-line-mode
  :init
  (defun ht/hl-line-mode ()
    (set-face-attribute 'hl-line nil :background "grey20"))
  (add-hook 'hl-line-mode-hook 'ht/hl-line-mode))

(use-package page-break-lines
  :ensure t
  :commands page-break-lines-mode
  :diminish page-break-lines-mode
  :config
  (setq page-break-lines-char ?-))

(use-package undo-tree
  :ensure t
  :commands undo-tree-mode
  :diminish undo-tree-mode)

(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward))

(defun ht/bind-xref-navigation-keys ()
  (bind-key "j" 'xref-next-line xref--xref-buffer-mode-map)
  (bind-key "k" 'xref-prev-line xref--xref-buffer-mode-map))

(add-hook 'xref--xref-buffer-mode-hook 'ht/bind-xref-navigation-keys)

(setq send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      mail-specify-envelope-from 't
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(provide 'feature-general)
