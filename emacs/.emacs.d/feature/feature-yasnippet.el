(use-package yasnippet
  :ensure t
  :defer 10
  :diminish yas-minor-mode
  :config
  (add-hook 'term-mode-hook (lambda ()
                              (setq yas-dont-activate t)))
  (setq yas-prompt-functions '(yas-ido-prompt))
  (yas-global-mode 1))

(provide 'feature-yasnippet)
