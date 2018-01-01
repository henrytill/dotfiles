(use-package company
  :ensure t
  :commands (company-mode global-company-mode)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends (remove 'company-clang company-backends)
        company-global-modes '(not eshell-mode)))

(provide 'feature-company)
