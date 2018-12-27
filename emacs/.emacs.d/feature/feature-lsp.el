(when (version<= "26.1" emacs-version)
  (use-package eglot
    :ensure t
    :commands eglot))

(provide 'feature-lsp)
