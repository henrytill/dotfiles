(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume))
  :init
  (use-package flx
    :ensure t)
  (use-package smex
    :ensure t)
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
  (setq enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t)
  (ht/comment
    ;; https://oremacs.com/2016/01/06/ivy-flx/
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
          ivy-initial-inputs-alist nil)
    nil)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(provide 'feature-ivy)
