(defun ht/haskell-mode ()
  (setq electric-indent-local-mode 0
        evil-auto-indent nil
        haskell-doc-prettify-types nil
        haskell-interactive-popup-errors nil)
  (define-key evil-normal-state-local-map (kbd "C-]") 'xref-find-definitions)
  (define-key evil-normal-state-local-map (kbd "C-t") 'xref-pop-marker-stack)
  nil)

(use-package dante
  :ensure t
  :defines (dante-project-root
            dante-repl-command-line
            dante-target)
  :commands dante-mode)

(use-package company-ghci
  :ensure t
  :disabled t
  :defer t
  :config
  (add-to-list 'company-backends 'company-ghci))

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'"      . haskell-mode)
         ("\\.hs-boot\\'" . haskell-mode)
         ("\\.hsc\\'"     . haskell-mode)
         ("\\.lhs\\'"     . literate-haskell-mode)
         ("\\.cabal\\'"   . haskell-cabal-mode))
  :init
  (ht/comment
    (add-hook 'haskell-mode-local-vars-hook 'dante-mode)
    (add-hook 'haskell-mode-hook 'flycheck-mode)
    nil)
  (dolist (mode '(company-mode
                  electric-pair-mode
                  haskell-indentation-mode
                  ht/haskell-mode
                  interactive-haskell-mode))
    (add-hook 'haskell-mode-hook mode)))

(provide 'feature-haskell)
