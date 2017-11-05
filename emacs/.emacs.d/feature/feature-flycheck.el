
(use-package flycheck-haskell
  :ensure t
  :disabled t
  :commands flycheck-haskell-setup)

(use-package flycheck-rust
  :ensure t
  :commands flycheck-rust-setup)

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :init
  (ht/comment
    (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
    nil)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-completion-system 'ido)
  (setq-default flycheck-disabled-checkers '(haskell-ghc
                                             haskell-hlint
                                             haskell-stack-ghc
                                             javascript-jslint)))

(provide 'feature-flycheck)
