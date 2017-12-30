(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.edn\\'"  . clojure-mode)
         ("\\.boot\\'" . clojure-mode))
  :init
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

(use-package lisp-mode
  :defer t
  :init
  (dolist (mode '(eldoc-mode enable-paredit-mode))
    (add-hook 'lisp-mode-hook mode)
    (add-hook 'emacs-lisp-mode-hook mode))
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-to-list 'magic-mode-alist '("#! emacs --script" . emacs-lisp-mode)))

(use-package paren-face
  :ensure t
  :defer t
  :config
  (setq paren-face-regexp "[][(){}]")
  (global-paren-face-mode))

(defun ht/scheme-mode ()
  (dolist (form+n '((conde . 0)
                    (fresh . 1)
                    (run   . 2)
                    (run*  . 1)))
    (put (car form+n) 'scheme-indent-function (cdr form+n))))

(use-package scheme
  :mode (("\\.scm\\'" . scheme-mode)
         ("\\.ss\\'"  . scheme-mode))
  :init
  (add-hook 'scheme-mode-hook 'ht/scheme-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode))

(provide 'feature-lisp)
