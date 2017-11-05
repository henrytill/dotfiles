(defun ht/lsp-flycheck-mode ()
  (interactive)
  (flycheck-mode 1)
  (when (not (featurep 'lsp-flycheck))
    (require 'lsp-flycheck))
  (setq-local flycheck-highlighting-mode nil))

(use-package lsp-mode
  :ensure t
  :disabled t
  :commands lsp-haskell-enable
  :init
  (add-hook 'lsp-mode-hook #'ht/lsp-flycheck-mode))

(use-package lsp-haskell
  :ensure t
  :disabled t)

(use-package company-lsp
  :ensure t
  :disabled t
  :config
  (add-to-list 'company-backends 'company-lsp))

(provide 'feature-lsp)
