(defun ht/haskell-mode ()
  (setq electric-indent-local-mode 0
        evil-auto-indent nil
        haskell-doc-prettify-types nil
        haskell-interactive-popup-errors nil)
  (define-key evil-normal-state-local-map (kbd "C-]") 'xref-find-definitions)
  (define-key evil-normal-state-local-map (kbd "C-t") 'xref-pop-marker-stack)
  nil)

(defun ht/haskell-interactive-wrapper (arg)
  "Prompt user to enter an additional argument to add to haskell-process-args-*"
  (interactive "sEnter argument: ")
  (dolist (args '(haskell-process-args-cabal-repl
                  haskell-process-args-cabal-new-repl
                  haskell-process-args-ghci
                  haskell-process-args-stack-ghci))
    (custom-reevaluate-setting args)
    (add-to-list args arg))
  (haskell-interactive-bring))

(defun ht/haskell-toggle-dante-mode ()
  (interactive)
  (let ((enable-dante-mode (lambda (x)
                             (if (bound-and-true-p dante-mode)
                                 (dante-mode x)
                               (with-current-buffer (current-buffer)
                                 (when (eq major-mode 'haskell-mode)
                                   (dante-mode x)))))))
    (if (member 'dante-mode haskell-mode-local-vars-hook)
        (progn
          (remove-hook 'haskell-mode-local-vars-hook 'dante-mode)
          (funcall enable-dante-mode 0))
      (progn
        (add-hook 'haskell-mode-local-vars-hook 'dante-mode)
        (funcall enable-dante-mode 1)))))

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
  (add-hook 'haskell-mode-local-vars-hook 'dante-mode)
  (dolist (mode '(company-mode
                  electric-pair-mode
                  flycheck-mode
                  haskell-indentation-mode
                  ht/haskell-mode
                  interactive-haskell-mode))
    (add-hook 'haskell-mode-hook mode)))

(provide 'feature-haskell)
