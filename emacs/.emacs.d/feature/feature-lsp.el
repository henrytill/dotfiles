(when (version<= "26.1" emacs-version)
  (use-package eglot
    :ensure t
    :init
    (setq eldoc-echo-area-use-multiline-p nil)
    :config
    (setq eglot-server-programs
          (ht/replace-item-in-alist eglot-server-programs 'rust-mode '("rust-analyzer")))))

(provide 'feature-lsp)
