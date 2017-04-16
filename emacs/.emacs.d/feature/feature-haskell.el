(defun ht/haskell-mode ()
  (setq electric-indent-local-mode 0
        evil-auto-indent nil))

(defun ht/haskell-interactive-wrapper (arg)
  "Prompt user to enter an additional argument to add to
haskell-process-args-cabal-repl"
  (interactive "sEnter argument: ")
  (add-to-list 'haskell-process-args-cabal-repl arg)
  (haskell-interactive-bring))

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'"      . haskell-mode)
         ("\\.hs-boot\\'" . haskell-mode)
         ("\\.lhs\\'"     . literate-haskell-mode)
         ("\\.cabal\\'"   . haskell-cabal-mode))
  :init
  (use-package flycheck-haskell
    :ensure t
    :defer t
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))
  (use-package company-ghci
    :ensure t
    :defer t
    :init
    (add-hook 'haskell-mode-hook 'company-mode)
    :config
    (add-to-list 'company-backends 'company-ghci))
  (dolist (mode '(electric-pair-mode
                  haskell-indentation-mode
                  ht/haskell-mode
                  interactive-haskell-mode))
    (add-hook 'haskell-mode-hook mode)))

(provide 'feature-haskell)
