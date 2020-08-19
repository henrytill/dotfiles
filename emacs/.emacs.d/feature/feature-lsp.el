(when (version<= "26.1" emacs-version)
  (use-package eglot
    :ensure t
    :commands eglot
    :init
    (setq eldoc-echo-area-use-multiline-p nil)))

(provide 'feature-lsp)
