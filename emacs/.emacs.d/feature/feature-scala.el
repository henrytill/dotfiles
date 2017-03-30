(defun ht/scala-mode ()
  (setq scala-indent:align-parameters t))

(defconst ht/flycheck-quickfix-load-path "~/src/flycheck-quickfix")

(use-package scala-mode
  :ensure t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'"   . scala-mode))
  :init
  (use-package flycheck-quickfix
    :if (lambda () (file-directory-p ht/flycheck-quickfix-load-path))
    :load-path ht/flycheck-quickfix-load-path
    :config
    (add-hook 'scala-mode-hook 'flycheck-quickfix-setup)
    (add-hook 'scala-mode-hook 'flycheck-mode))
  (add-hook 'scala-mode-hook 'ht/scala-mode)
  (add-hook 'scala-mode-hook 'auto-revert-mode)
  (add-hook 'scala-mode-hook 'electric-pair-mode))

(provide 'feature-scala)
