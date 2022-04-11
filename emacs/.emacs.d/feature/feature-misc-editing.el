(use-package capnp-mode
  :if (locate-file "capnp-mode.el" load-path)
  :mode "\\.capnp\\'"
  :init
  (defun ht/capnp-mode ()
    (setq-local tab-width 2))
  (add-hook 'capnp-mode-hook #'ht/capnp-mode))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\(?:\\..*\\)?\\'")

(use-package pandoc-mode
  :ensure t
  :commands pandoc-mode)

(defun ht/truncate-lines ()
  (interactive)
  (toggle-truncate-lines 1))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'ht/truncate-lines)
  (add-hook 'markdown-mode-hook #'ht/hide-lines-tail-display)
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
