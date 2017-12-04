(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

(use-package pandoc-mode
  :ensure t
  :commands pandoc-mode)

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'whitespace-mode))

(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'" . powershell-mode))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml'")

(provide 'feature-misc-editing)
