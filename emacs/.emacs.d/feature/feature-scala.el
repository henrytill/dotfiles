(defun ht/scala-mode ()
  (setq scala-font-lock:var-face 'font-lock-variable-name-face
        scala-indent:align-parameters t))

(use-package scala-mode
  :ensure t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'"   . scala-mode))
  :init
  (use-package flycheck-quickfix
    :load-path "site-lisp/flycheck-quickfix"
    :commands flycheck-quickfix-setup
    :init
    (add-hook 'scala-mode-hook 'flycheck-quickfix-setup)
    (add-hook 'scala-mode-hook 'flycheck-mode))
  (add-hook 'scala-mode-hook 'ht/scala-mode)
  (add-hook 'scala-mode-hook 'auto-revert-mode)
  (add-hook 'scala-mode-hook 'electric-pair-mode))

(provide 'feature-scala)
