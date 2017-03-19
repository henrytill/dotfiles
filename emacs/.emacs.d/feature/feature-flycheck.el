(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :config
  (setq flycheck-completion-system 'ido)
  (setq-default flycheck-disabled-checkers '(javascript-jslint)))

(provide 'feature-flycheck)
