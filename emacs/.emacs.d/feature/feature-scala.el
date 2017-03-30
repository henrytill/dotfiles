(defun ht/scala-mode ()
  (setq scala-indent:align-parameters t))

(use-package scala-mode
  :ensure t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'"   . scala-mode))
  :init
  (let ((lp "~/src/flycheck-quickfix"))
    (use-package flycheck-quickfix
      :if (lambda () (file-directory-p lp))
      :load-path lp
      :config
      (add-hook 'scala-mode-hook 'flycheck-quickfix-setup)
      (add-hook 'scala-mode-hook 'flycheck-mode)))
  (add-hook 'scala-mode-hook 'ht/scala-mode)
  (add-hook 'scala-mode-hook 'auto-revert-mode)
  (add-hook 'scala-mode-hook 'electric-pair-mode))

(provide 'feature-scala)
