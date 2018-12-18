(defun ht/forth-mode ()
  (setq forth-indent-level 4
        forth-minor-indent-level 2
        forth-hilight-level 3))

(use-package forth-mode
  :if (executable-find "gforth")
  :mode "\\.fs\\'"
  :defines (forth-indent-level forth-minor-indent-level forth-hilight-level)
  :init
  (autoload 'forth-mode "gforth.el")
  (add-hook 'forth-mode-hook 'ht/forth-mode))

(use-package forth-block-mode
  :if (executable-find "gforth")
  :mode "\\.fb\\'"
  :init
  (autoload 'forth-block-mode "gforth.el"))

(provide 'feature-forth)
