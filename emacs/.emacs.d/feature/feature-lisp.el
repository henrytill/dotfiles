(defun ht/rkt-predicate ()
  (and (buffer-file-name)
       (string-equal (file-name-extension (buffer-file-name)) "rkt")))

(defun ht/setup-rkt-checker ()
  (flycheck-define-checker racket-alt
    "A Racket syntax checker using the Racket compiler. See URL `http://racket-lang.org/'."
    :command ("racket" "-f" source-inplace)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
    :modes scheme-mode
    :predicate ht/rkt-predicate)
  (add-to-list 'flycheck-checkers 'racket-alt))

(defun ht/scheme-mode ()
  (dolist (form+n '((conde . 0)
                    (fresh . 1)
                    (run   . 2)
                    (run*  . 1)))
    (put (car form+n) 'scheme-indent-function (cdr form+n))))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.edn\\'"  . clojure-mode)
         ("\\.boot\\'" . clojure-mode))
  :init
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

(use-package geiser
  :load-path "site-lisp/geiser/elisp"
  :defer t
  :defines geiser-active-implementations
  :commands geiser-mode
  :config
  (setq geiser-active-implementations '(racket)
        geiser-default-implementation 'racket))

(use-package ielm
  :commands ielm
  :init
  (add-hook 'ielm-mode-hook 'company-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode))

(use-package lisp-mode
  :defer t
  :init
  (add-hook 'lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-to-list 'magic-mode-alist '("#! emacs --script" . emacs-lisp-mode)))

(use-package paren-face
  :ensure t
  :defer t
  :config
  (setq paren-face-regexp "[][(){}]")
  (global-paren-face-mode))

(use-package scheme
  :mode (("\\.rkt\\'" . scheme-mode)
         ("\\.scm\\'" . scheme-mode))
  :init
  (add-hook 'scheme-mode-hook 'ht/scheme-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  :config
  (when (executable-find "plt-r5rs")
    (setq scheme-program-name "plt-r5rs")))

(provide 'feature-lisp)
