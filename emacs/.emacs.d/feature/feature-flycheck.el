(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :init
  (use-package flycheck-haskell
    :ensure t
    :commands flycheck-haskell-setup)
  (use-package flycheck-rust
    :ensure t
    :commands flycheck-rust-setup)
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  :config
  (setq flycheck-completion-system 'ido)
  (setq-default flycheck-disabled-checkers '(javascript-jslint)))

(provide 'feature-flycheck)
