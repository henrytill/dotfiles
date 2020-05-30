(defun ht/haskell-mode ()
  (setq electric-indent-local-mode 0
        evil-auto-indent nil
        haskell-doc-prettify-types nil
        haskell-interactive-popup-errors nil
        haskell-process-log t
        haskell-process-type 'cabal-new-repl
        whitespace-line-column 120))

(defun ht/haskell-wrapper-function-nix ()
  (interactive)
  (setq haskell-process-wrapper-function
        (lambda (argv) (append (list "nix-shell" "--command" )
                               (list (mapconcat 'identity argv " "))))))

(defun ht/haskell-wrapper-function-identity ()
  (interactive)
  (setq haskell-process-wrapper-function 'identity))

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'"      . haskell-mode)
         ("\\.hs-boot\\'" . haskell-mode)
         ("\\.hsc\\'"     . haskell-mode)
         ("\\.lhs\\'"     . literate-haskell-mode)
         ("\\.cabal\\'"   . haskell-cabal-mode))
  :init
  (dolist (mode '(company-mode
                  electric-pair-mode
                  haskell-indentation-mode
                  ht/haskell-mode
                  interactive-haskell-mode))
    (add-hook 'haskell-mode-hook mode)))

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (ht/comment
    (add-hook 'haskell-mode-hook 'flymake-mode)
    (add-hook 'haskell-mode-hook 'dante-mode)
    nil))

(provide 'feature-haskell)
