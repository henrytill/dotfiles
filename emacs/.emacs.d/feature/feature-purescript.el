(defun ht/purescript-mode ()
  (setq electric-indent-local-mode 0))

(defun ht/psc-ide-mode ()
  (psc-ide-mode 1)
  (company-mode 1)
  (define-key evil-normal-state-local-map (kbd "C-]") 'psc-ide-goto-definition))

(use-package psc-ide
  :if (executable-find "psc-ide-server")
  :ensure t
  :commands psc-ide-mode
  :init
  (ht/comment
    (add-hook 'purescript-mode-hook 'ht/psc-ide-mode)
    nil))

(use-package purescript-mode
  :ensure t
  :mode "\\.purs\\'"
  :init
  (add-hook 'purescript-mode-hook 'purescript-indentation-mode)
  (add-hook 'purescript-mode-hook 'ht/purescript-mode))

(provide 'feature-purescript)
