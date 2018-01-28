(defun ht/haskell-mode ()
  (setq electric-indent-local-mode 0
        evil-auto-indent nil
        haskell-doc-prettify-types nil
        haskell-interactive-popup-errors nil
        haskell-process-log t
        whitespace-line-column 120)
  (define-key evil-normal-state-local-map (kbd "C-]") 'xref-find-definitions)
  (define-key evil-normal-state-local-map (kbd "C-t") 'xref-pop-marker-stack)
  nil)

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

;; https://github.com/haskell/haskell-mode/pull/1573
(with-eval-after-load 'haskell-load
  (defun haskell-process-extract-modules (buffer)
    "Extract the modules from the process buffer. (fixed)"
    (let* ((modules-string (match-string 1 buffer))
           (modules (and modules-string (split-string modules-string ", "))))
      (cons modules modules-string)))
  nil)

(use-package dante
  :ensure t
  :disabled t
  :defines (dante-project-root
            dante-repl-command-line
            dante-target)
  :commands dante-mode
  :requires (haskell-mode)
  :init
  (add-hook 'haskell-mode-local-vars-hook 'dante-mode)
  (add-hook 'nix-buffer-after-load-hook 'dante-restart)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(use-package company-ghci
  :ensure t
  :disabled t
  :defer t
  :config
  (add-to-list 'company-backends 'company-ghci))

(provide 'feature-haskell)
