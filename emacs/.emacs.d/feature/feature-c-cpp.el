(with-eval-after-load 'cc-styles
  (c-add-style "stevens" '("bsd" (c-basic-offset . 4)))
  (c-add-style "hnf"     '("bsd" (c-basic-offset . 2)))
  (setq c-default-style "hnf"))

(defun ht/c++-mode ()
  (c-set-style "stroustrup"))

(eval-and-compile
  (defun ht/rtags-load-path ()
    (letrec ((rtags-site-lisp  (expand-directory-name "../../share/emacs/site-lisp/rtags"
                                                      (executable-find "rdm"))))
      (when (file-directory-p rtags-site-lisp)
        rtags-site-lisp))))

(defun ht/rtags-mode ()
  (rtags-start-process-unless-running)
  (define-key evil-normal-state-local-map (kbd "C-]") 'rtags-find-symbol-at-point)
  (define-key evil-normal-state-local-map (kbd "C-t") 'rtags-location-stack-back))

(defun ht/flycheck-rtags-mode ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  (flycheck-mode 1))

(use-package clang-format
  :ensure t
  :commands (clang-format
             clang-format-buffer
             clang-format-region))

(use-package rtags
  :if (executable-find "rdm")
  :defines rtags-start-process-unless-running
  :commands rtags-start-process-unless-running
  :load-path (lambda () (ht/rtags-load-path))
  :init
  (use-package flycheck-rtags
    :load-path (lambda () (ht/rtags-load-path)))
  (use-package company-rtags
    :disabled t
    :load-path (lambda () (ht/rtags-load-path)))
  (add-hook 'c-mode-hook 'ht/rtags-mode)
  (add-hook 'c-mode-hook 'ht/flycheck-rtags-mode)
  (add-hook 'c++-mode-hook 'ht/rtags-mode)
  (add-hook 'c++-mode-hook 'ht/flycheck-rtags-mode))

(use-package irony
  :ensure t
  :commands irony-mode
  :config
  (use-package flycheck-irony
    :disabled t
    :ensure t
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))
  (use-package company-irony
    :ensure t
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-irony)))
  (use-package irony-eldoc
    :ensure t
    :init
    (add-hook 'irony-mode-hook 'irony-eldoc))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode))

(use-package c-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode))
  :init
  (add-hook 'c-mode-hook 'electric-pair-mode))

(use-package c++-mode
  :mode (("\\.cc\\'"  . c++-mode)
         ("\\.cpp\\'" . c++-mode))
  :init
  (add-hook 'c++-mode-hook 'ht/c++-mode)
  (add-hook 'c++-mode-hook 'electric-pair-mode))

(provide 'feature-c-cpp)
