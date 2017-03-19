(defun ht/scala-mode ()
  (setq scala-indent:align-parameters t))

(use-package scala-mode
  :ensure t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'"   . scala-mode))
  :init
  (add-hook 'scala-mode-hook 'ht/scala-mode)
  (add-hook 'scala-mode-hook 'auto-revert-mode)
  (add-hook 'scala-mode-hook 'electric-pair-mode))

(use-package sbt-mode
  :ensure t
  :commands sbt-start)

(provide 'feature-scala)
