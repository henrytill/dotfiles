(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-setup)

(use-package flycheck-rust
  :ensure t
  :commands flycheck-rust-setup)

(use-package flycheck
  :ensure t
  :commands (flycheck-mode
             flycheck-list-errors
             flycheck-next-error
             flycheck-previous-error)
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-completion-system 'ido)
  (setq-default flycheck-disabled-checkers '(haskell-ghc
                                             haskell-hlint
                                             haskell-stack-ghc
                                             javascript-jslint)))

(defhydra ht/hydra-flycheck (:idle 1.0)
  "
flycheck
--------
_l_: flycheck-list-errors
_j_: flycheck-next-error
_k_: flycheck-previous-error
_v_: flycheck-verify-setup
"
  ("l" flycheck-list-errors     nil :exit t)
  ("j" flycheck-next-error      nil :exit t)
  ("k" flycheck-previous-errror nil :exit t)
  ("v" flycheck-verify-setup    nil :exit t))

(provide 'feature-flycheck)
