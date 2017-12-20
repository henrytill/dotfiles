(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume))
  :init
  (use-package counsel
    :ensure t
    :bind (("M-x"     . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           ("<f1> f"  . counsel-describe-function)
           ("<f1> v"  . counsel-describe-variable)
           ("<f1> l"  . counsel-find-library)
           ("<f2> i"  . counsel-info-lookup-symbol)
           ("<f2> u"  . counsel-unicode-char)))
  (use-package swiper
    :ensure t
    :bind ("C-s" . swiper))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(provide 'feature-ivy)
