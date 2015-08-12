;;; utility functions

(defun is-darwin-p ()
  (string-equal system-type "darwin"))

(defun is-linux-p ()
  (string-equal system-type "gnu/linux"))

(defun el-which (cmd)
  (replace-regexp-in-string "\\\n" ""
                            (shell-command-to-string
                             (concat "type -p " cmd))))

(defun shell-command-p (cmd)
  (let ((result (el-which cmd)))
    (> (length result) 0)))

(defun expand-directory-name (dir &optional parent-dir)
  (file-name-as-directory (expand-file-name dir parent-dir)))


;;; basic settings

(setq apropos-do-all t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      browse-url-browser-function 'browse-url-default-browser
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      doc-view-resolution 300
      epa-armor t
      gnutls-min-prime-bits 1024
      ido-handle-duplicate-virtual-buffers 2
      ido-use-virtual-buffers t
      inhibit-startup-message t
      load-prefer-newer t
      mouse-yank-at-point t
      org-directory "~/org"
      require-final-newline t
      ring-bell-function 'ignore
      save-interprogram-paste-before-kill t
      save-place-file (concat user-emacs-directory "places")
      scroll-conservatively 1
      visible-bell t
      x-select-enable-primary t
      x-select-enable-clipboard t)

(setq-default indent-tabs-mode nil
              ispell-program-name "aspell")

(load custom-file t)

(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")   'isearch-forward)
(global-set-key (kbd "C-M-r")   'isearch-backward)


;;; package.el

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))


;;; load-path

(mapc #'(lambda (path)
          (push (expand-file-name path user-emacs-directory) load-path))
      '("site-lisp" "site-lisp/use-package"))

(let ((nix-site-lisp (expand-directory-name "~/.nix-profile/share/emacs/site-lisp/")))
  (when (file-directory-p nix-site-lisp)
    (add-to-list 'load-path nix-site-lisp)))


;;; packages

(require 'use-package)
(require 'bind-key)
(require 'diminish "diminish-0.44.el")

(use-package clojure-mode        :ensure t)
(use-package dash                :ensure t)
(use-package idle-highlight-mode :ensure t)
(use-package pkg-info            :ensure t)
(use-package queue               :ensure t)
(use-package sml-mode            :ensure t)

(use-package eldoc
  :diminish eldoc-mode
  :config
  (add-hook 'cider-mode-hook            'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook       'eldoc-mode)
  (add-hook 'ielm-mode-hook             'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'clojure-mode-hook    'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook       'paredit-mode)
  (add-hook 'lisp-mode-hook       'paredit-mode)
  (add-hook 'scheme-mode-hook     'paredit-mode))

(use-package cider
  :load-path "site-lisp/cider"
  :bind (("C-c M-c" . cider-connect)
         ("C-c M-j" . cider-jack-in))
  :init
  (use-package cl)
  :config
  (use-package cider-apropos)
  (use-package cider-browse-ns)
  (use-package cider-classpath)
  (use-package cider-grimoire)
  (use-package cider-inspector)
  (use-package cider-macroexpansion)
  (use-package cider-scratch)
  (use-package cider-selector)
  (remove-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
  (setq cider-show-error-buffer 'except-in-repl))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode 1)
  (setq company-global-modes '(not eshell-mode)))

(use-package compile
  :config
  (use-package ansi-color)
  (defun ht-display-ansi-colors ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'ht-display-ansi-colors))

(use-package erc
  :defer t
  :config
  (setq erc-fill-function 'erc-fill-static
        erc-fill-static-center 19
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-prompt ">"
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (use-package erc-hl-nicks :ensure t)
  (require 'erc-spelling)
  (add-to-list 'erc-modules 'hl-nicks)
  (add-to-list 'erc-modules 'spelling))

(use-package eshell
  :config
  (setq eshell-prompt-function
        (lambda nil
          (concat "\n"
                  (user-login-name) "@"
                  (abbreviate-file-name (eshell/pwd)) "> "))
        eshell-prompt-regexp "^[^>]*> ")
  (add-hook 'eshell-prompt-load-hook
            (defun my-color-eshell-prompt ()
              (set-face-foreground 'eshell-prompt "#2aa198")))
  (add-hook 'eshell-mode-hook
            (defun my-eshell-visual-commands ()
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "bash")))
  (defun eshell-term ()
    (interactive)
    (eshell)
    (setq-local mode-line-format nil))
  (defun eshell/clear ()
    "Clear the Eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (defun eshell/rgrep (&rest args)
    "Use Emacs grep facility instead of calling external grep."
    (eshell-grep "rgrep" args t))
  (defun eshell/scheme ()
    (interactive)
    (call-interactively 'run-scheme))
  (defalias 'eshell/view 'view-file))

(use-package exec-path-from-shell
  :if (is-darwin-p)
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)
  (setq flx-ido-use-faces nil))

(use-package flycheck
  :ensure t
  :preface
  (defun ht-rkt-predicate ()
    (and (buffer-file-name)
         (string-equal (file-name-extension (buffer-file-name)) "rkt")))
  :config
  (flycheck-define-checker racket-alt
    "A Racket syntax checker using the Racket compiler. See URL `http://racket-lang.org/'."
    :command ("racket" "-f" source-inplace)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
    :modes scheme-mode
    :predicate ht-rkt-predicate)
  (add-to-list 'flycheck-checkers 'racket-alt)
  (setq flycheck-completion-system nil))

(use-package geiser
  :load-path "site-lisp/geiser/elisp")

(use-package haskell-mode
  :ensure t
  :config
  (use-package haskell-style
    :config
    (add-hook 'haskell-mode-hook 'haskell-style))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package helm-config
  :disabled t
  :ensure helm
  :demand t
  :bind (("C-c h" . helm-command-prefix))
  :config
  (use-package helm)
  (use-package helm-mode
    :diminish helm-mode
    :config
    (helm-mode 1))
  (use-package helm-adaptive
    :config
    (helm-adaptive-mode 1))
  (global-unset-key (kbd "C-x c"))
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")   'helm-select-action)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)))

(use-package hs-minor-mode
  :bind (("<f5>"     . hs-toggle-hiding)
         ("M-<f5>"   . hs-hide-all)
         ("M-S-<f5>" . hs-show-all)))

(use-package ido
  :config
  (setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
                          " [No match]" " [Matched]" " [Not readable]"
                          " [Too big]" " [Confirm]")
        ido-enable-flex-matching t)
  (add-hook 'ido-minibuffer-setup-hook
            (defun ido-disable-line-truncation ()
              (set (make-local-variable 'truncate-lines) nil)))
  (defun ht-ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ht-ido-define-keys)
  (ido-mode t))

(use-package inf-clojure
  :ensure t
  :config
  (setq inf-clojure-program "lein trampoline run -m clojure.main")
  (defun ht-revert-clojure-buffer ()
    (let ((ext (file-name-extension buffer-file-name)))
      (when (or (string-equal ext "clj")
                (string-equal ext "cljs")
                (string-equal ext "cljc"))
        (revert-buffer))))
  (defun ht-load-inf-clojure ()
    (interactive)
    (setq cider-auto-mode nil)
    (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
    (ht-revert-clojure-buffer))
  (defun ht-unload-inf-clojure ()
    (interactive)
    (remove-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
    (setq cider-auto-mode t)
    (ht-revert-clojure-buffer)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-completion-use-ido t
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t)
  (when (file-directory-p org-directory)
    (let* ((notes-file
            (expand-file-name "notes.org" org-directory))
           (notes-template
            `("n" "Notes" entry (file ,notes-file) "* %?\n  %i\n  %a"))
           (todo-file
            (expand-file-name "todo.org" org-directory))
           (todo-template
            `("t" "Todo" entry (file+headline ,todo-file "Tasks") "* TODO %?\n  %i\n  %a")))
      (setq org-agenda-files (list org-directory)
            org-capture-templates (list notes-template
                                        todo-template)
            org-default-notes-file notes-file)
      (set-register ?n `(file . ,(expand-file-name "notes.org" org-directory)))))
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (scheme . t))))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config
  (setq page-break-lines-char ?-)
  (add-hook 'prog-mode-hook 'page-break-lines-mode))

(use-package paren-face
  :ensure t
  :config
  (setq paren-face-regexp "[][(){}]")
  (global-paren-face-mode))

(use-package projectile
  :load-path "site-lisp/projectile"
  :diminish projectile-mode
  :config
  (use-package helm-projectile
    :disabled t
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))
  (projectile-global-mode))

(use-package saveplace
  :config
  (setq-default save-place t))

(use-package scheme
  :config
  (when (executable-find "plt-r5rs")
    (setq scheme-program-name "plt-r5rs")))

(use-package slime
  :load-path "site-lisp/slime"
  :defer t
  :commands slime
  :config
  (use-package slime-autoloads)
  (slime-setup '(slime-fancy slime-banner))
  (let ((ccl-loc  (executable-find "ccl64"))
        (sbcl-loc (executable-find "sbcl")))
    (when ccl-loc
      (if (boundp 'slime-lisp-implementations)
          (setq slime-lisp-implementations
                (cons `(ccl (,ccl-loc)) slime-lisp-implementations))
        (setq slime-lisp-implementations `((ccl (,ccl-loc))))
        (setq slime-default-lisp 'ccl)))
    (when sbcl-loc
      (if (boundp 'slime-lisp-implementations)
          (setq slime-lisp-implementations
                (cons `(sbcl (,sbcl-loc)) slime-lisp-implementations))
        (setq slime-lisp-implementations `((sbcl (,sbcl-loc)))))
      (setq slime-default-lisp 'sbcl)))
  (let* ((quicklisp-loc  (expand-directory-name "~/quicklisp"))
         (clhs-use-local (expand-file-name "clhs-use-local.el" quicklisp-loc)))
    (when (file-exists-p clhs-use-local)
      (load-file clhs-use-local))))

(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

(use-package tuareg
  :ensure t
  :config
  (when (and (executable-find "opam")
             (file-directory-p "~/.opam"))
    (let ((opam-env (car (read-from-string (shell-command-to-string
                                            "opam config env --sexp")))))
      (dolist (var opam-env)
        (setenv (car var) (cadr var))))
    (let ((toplevel (expand-directory-name "../../share/emacs/site-lisp"
                                           (getenv "OCAML_TOPLEVEL_PATH"))))
      (when (file-directory-p toplevel)
        (add-to-list 'load-path toplevel))))
  (when (and (executable-find "utop")
             (locate-file "utop.el" load-path))
    (autoload 'utop "utop" "Toplevel for OCaml" t)
    (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
    (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (add-hook 'prog-mode-hook 'undo-tree-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face tabs lines-tail trailing empty)
        whitespace-line-column 80)
  (add-hook 'prog-mode-hook 'whitespace-mode))


;;; cosmetics

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(show-paren-mode 1)

;;; frame titles
(setq frame-title-format
      '("" invocation-name ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; mode line
(set-face-attribute 'mode-line          nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'header-line        nil :box nil)

;;; column numbers
(column-number-mode 1)


;;; cursor
(setq visible-cursor nil)
(setq-default cursor-type 'box)

;;; hl-line
(defun ht-select-line-mode ()
  (hl-line-mode 1)
  (setq cursor-type 'nil))

(add-hook 'dired-mode-hook        'ht-select-line-mode)
(add-hook 'ibuffer-mode-hook      'ht-select-line-mode)
(add-hook 'gnus-group-mode-hook   'ht-select-line-mode)
(add-hook 'gnus-summary-mode-hook 'ht-select-line-mode)
(add-hook 'gnus-server-mode-hook  'ht-select-line-mode)
(add-hook 'package-menu-mode-hook 'ht-select-line-mode)

;;; prettify symbols
(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 4))
  (global-prettify-symbols-mode)
  (defun scheme-prettify-symbols ()
    (push '("lambda" . "\u03bb") prettify-symbols-alist))
  (add-hook 'scheme-mode-hook 'scheme-prettify-symbols))

;;; truncate lines
(defun ht-toggle-truncate-lines ()
  (setq truncate-lines t))

(add-hook 'compilation-mode-hook 'ht-toggle-truncate-lines)
(add-hook 'dired-mode-hook       'ht-toggle-truncate-lines)
(add-hook 'shell-mode-hook       'ht-toggle-truncate-lines)

;;; warning keywords
(defun ht-add-watchwords ()
  (font-lock-add-keywords nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\)" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'ht-add-watchwords)


;;; misc. programming

;;; c
(setq c-default-style '((java-mode . "java")
                        (awk-mode  . "awk")
                        (c-mode    . "k&r")
                        (other     . "gnu")))

(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook 'electric-pair-mode)

;;; supercollider
(when (and (is-darwin-p)
           (executable-find "sclang")
           (file-directory-p (expand-directory-name "~/src/other/scel")))
  (add-to-list 'load-path (expand-directory-name "~/src/other/scel"))
  (require 'sclang)
  (add-hook 'sclang-mode-hook 'electric-pair-mode))


;;; platform-specific settings

;;; darwin
(when (and (is-darwin-p) (window-system))
  (let ((ansi-term  (expand-file-name "ansi-term" user-emacs-directory))
        (aspell-dir (expand-directory-name "~/.nix-profile/lib/aspell/"))
        (mplus-font (expand-file-name "mplus-1mn-regular.ttf" "~/Library/Fonts")))
    (setq explicit-shell-file-name ansi-term
          mac-command-modifier 'super
          mac-option-modifier 'meta)
    (when (file-directory-p aspell-dir)
      (setenv "ASPELL_CONF" (concat "dict-dir " aspell-dir)))
    (when (file-exists-p mplus-font)
      (set-face-attribute 'default nil :font "M+ 1mn 14"))
    (add-to-list 'default-frame-alist '(height . 40))
    (add-to-list 'default-frame-alist '(width . 100))
    (defun ht-reset-frame ()
      (interactive)
      (let ((height (cdr (assq 'height default-frame-alist)))
            (width  (cdr (assq 'width  default-frame-alist))))
        (set-frame-height (selected-frame) height)
        (set-frame-width  (selected-frame) width)))))

;;; nixos
(when (and (is-linux-p) (file-directory-p "/etc/nixos"))
  (require 'tramp)
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
  (set-face-attribute 'region nil :background "lightgoldenrod2")
  (set-face-attribute 'region nil :foreground "black"))


;;; registers

(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))
