;;; pre init

(defconst emacs-start-time (current-time))


;;; utility functions & macros

(eval-and-compile
  (defun is-darwin-p ()
    (string-equal system-type "darwin"))
  (defun is-linux-p ()
    (string-equal system-type "gnu/linux")))

(defun expand-directory-name (dir &optional parent-dir)
  (file-name-as-directory (expand-file-name dir parent-dir)))

(defmacro ht-comment (&rest body)
  "Comment out one or more s-expressions."
  nil)


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
(global-set-key (kbd "M-%")     'query-replace-regexp)
(global-set-key (kbd "C-M-%")   'query-replace)


;;; package.el

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))


;;; load-path

(eval-and-compile
  (mapc #'(lambda (path)
            (push (expand-file-name path user-emacs-directory) load-path))
        '("site-lisp" "site-lisp/use-package"))
  (let ((nix-site-lisp (expand-directory-name "~/.nix-profile/share/emacs/site-lisp/")))
    (when (file-directory-p nix-site-lisp)
      (add-to-list 'load-path nix-site-lisp))))


;;; other paths

(eval-and-compile
  (defun ht-oz-home ()
    (cond ((is-darwin-p) "/Applications/Mozart2.app/Contents/Resources")
          ((is-linux-p)  "~/.nix-profile")))
  (defun ht-oz-load-path ()
    (cond ((is-darwin-p) "~/src/other/mozart-elisp")
          ((is-linux-p)  (expand-directory-name "share/mozart/elisp" (ht-oz-home))))))


;;; packages

(require 'use-package)
(require 'bind-key)
(require 'diminish "diminish-0.44.el")

(use-package dash                :ensure t :defer t)
(use-package idle-highlight-mode :ensure t :defer t)
(use-package pkg-info            :ensure t :defer t)
(use-package queue               :ensure t :defer t)

(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :diminish paredit-mode)

(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window))

(use-package avy
  :ensure t
  :bind (("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :init
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "C-'") 'avy-isearch)))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.edn\\'"  . clojure-mode)
         ("\\.boot\\'" . clojure-mode))
  :init
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

(use-package cider
  :load-path "site-lisp/cider"
  :bind (("C-c M-c" . cider-connect)
         ("C-c M-j" . cider-jack-in))
  :init
  (use-package cl)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  :config
  (use-package cider-apropos)
  (use-package cider-browse-ns)
  (use-package cider-classpath)
  (use-package cider-grimoire)
  (use-package cider-inspector)
  (use-package cider-macroexpansion)
  (use-package cider-scratch)
  (use-package cider-selector)
  (setq cider-show-error-buffer 'except-in-repl))

(use-package company
  :ensure t
  :commands company-mode
  :diminish company-mode
  :config
  (setq company-global-modes '(not eshell-mode)))

(use-package compile
  :commands compile
  :init
  (use-package ansi-color)
  (defun ht-display-ansi-colors ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'ht-display-ansi-colors))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode)

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
  :commands (eshell eshell-command)
  :defines eshell-visual-commands
  :functions (eshell/pwd eshell-grep)
  :init
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
  (defun eshell/clear ()
    "Clear the Eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (defun eshell/rgrep (&rest args)
    "Use Emacs grep facility instead of calling external grep."
    (eshell-grep "rgrep" args t))
  (defalias 'eshell/view 'view-file))

(use-package exec-path-from-shell
  :if (is-darwin-p)
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package evil
  :ensure t
  :init
  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "g;" 'evil-avy-goto-char)
    (evil-leader/set-key "g'" 'evil-avy-goto-char-2)
    (evil-leader/set-key "gw" 'evil-avy-goto-word-1)
    (evil-leader/set-key "gl" 'evil-avy-goto-line)
    (global-evil-leader-mode))
  :config
  (delete 'term-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'term-mode)
  (evil-mode 1))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)
  (setq flx-ido-use-faces nil))

(use-package flycheck
  :ensure t
  :defer 5
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
  (setq flycheck-completion-system 'ido))

(use-package forth-mode
  :if (executable-find "gforth")
  :mode "\\.fs\\'"
  :init
  (autoload 'forth-mode "gforth.el")
  (defun ht-forth-mode ()
    (setq forth-indent-level 4
          forth-minor-indent-level 2
          forth-hilight-level 3))
  (add-hook 'forth-mode-hook 'ht-forth-mode))

(use-package forth-block-mode
  :if (executable-find "gforth")
  :mode "\\.fb\\'"
  :init
  (autoload 'forth-block-mode "gforth.el"))

(use-package geiser
  :load-path "site-lisp/geiser/elisp"
  :defines geiser-active-implementations
  :config
  (setq geiser-active-implementations '(racket)))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\(c\\|-boot\\)?\\'"
  :init
  (defun ht-haskell-style ()
    (setq tab-width 4
          haskell-indentation-layout-offset 4
          haskell-indentation-left-offset 4
          haskell-indentation-ifte-offset 4))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'ht-haskell-style))

(use-package hideshow
  :bind (("<f5>"     . hs-toggle-hiding)
         ("M-<f5>"   . hs-hide-all)
         ("M-S-<f5>" . hs-show-all))
  :functions ht-hs-add-xml-mode
  :config
  (defun ht-hs-add-xml-mode (mode forward-sexp-func)
    (let ((start         "<!--\\|<[^/>]*[^/]>")
          (end           "-->\\|</[^/>]*[^/]>")
          (comment-start "<!--"))
      (push (list mode start end comment-start forward-sexp-func nil)
            hs-special-modes-alist)))
  (ht-hs-add-xml-mode 'nxml-mode 'nxml-forward-element)
  (ht-hs-add-xml-mode 'html-mode 'sgml-skip-tag-forward)
  (ht-hs-add-xml-mode 'sgml-mode 'sgml-skip-tag-forward))

(use-package ido
  :config
  (ht-comment
   "Vertical Ido Results"
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
   nil)
  (ido-mode t))

(use-package ielm
  :commands ielm
  :init
  (add-hook 'ielm-mode-hook 'company-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-global-externs '("module" "require")))

(use-package lisp-mode
  :init
  (add-hook 'lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package org
  :load-path ("site-lisp/org-mode/lisp"
              "site-lisp/org-mode/contrib/lisp")
  :bind (("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :defer 30
  :functions org-bookmark-jump-unhide
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
                                                           (haskell . t)
                                                           (oz . t)
                                                           (scheme . t))))

(use-package oz
  :load-path (lambda () (list (ht-oz-load-path)))
  :mode ("\\.oz\\'" . oz-mode)
  :commands run-oz
  :init
  (setenv "OZHOME" (ht-oz-home))
  (add-hook 'oz-mode-hook 'electric-pair-mode)
  (add-hook 'oz-mode-hook 'page-break-lines-mode)
  (add-hook 'oz-mode-hook 'undo-tree-mode)
  (add-hook 'oz-mode-hook 'whitespace-mode))

(use-package page-break-lines
  :ensure t
  :commands page-break-lines-mode
  :diminish page-break-lines-mode
  :config
  (setq page-break-lines-char ?-))

(use-package paren-face
  :ensure t
  :config
  (setq paren-face-regexp "[][(){}]")
  (global-paren-face-mode))

(use-package prog-mode
  :defer t
  :init
  (defun ht-prog-mode ()
    (setq indicate-empty-lines 1))
  (add-hook 'prog-mode-hook 'ht-prog-mode)
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (add-hook 'prog-mode-hook 'undo-tree-mode)
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package projectile
  :load-path "site-lisp/projectile"
  :functions projectile-global-mode
  :diminish projectile-mode
  :defer 5
  :config
  (projectile-global-mode))

(use-package saveplace
  :config
  (setq-default save-place t))

(use-package scheme
  :init
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  :config
  (when (executable-find "plt-r5rs")
    (setq scheme-program-name "plt-r5rs")))

(use-package slime
  :load-path "site-lisp/slime"
  :defer t
  :commands slime
  :functions slime-setup
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

(use-package sml-mode
  :ensure t
  :mode "\\.sml\\'")

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :defines (TeX-view-program-list TeX-view-program-selection)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil)
  (when (and (is-linux-p) (executable-find "mupdf-x11"))
    (eval-after-load 'tex
      '(progn (add-to-list 'TeX-view-program-list
                           '("mupdf" ("mupdf-x11" (mode-io-correlate " -p %(outpage)") " %o")))
              (add-to-list 'TeX-view-program-selection
                           '(output-pdf "mupdf"))))))

(use-package tuareg
  :mode (("\\.ml[ip]?\\'" . tuareg-mode)
         ("\\.eliomi?\\'" . tuareg-mode))
  :init
  (defun ht-opam-config-env ()
    (when (executable-find "opam")
      (dolist (var (car (read-from-string (shell-command-to-string
                                           "opam config env --sexp"))))
        (setenv (car var) (cadr var)))))
  (defun ht-opam-load-path ()
    (let ((ocaml-toplevel-path (getenv "OCAML_TOPLEVEL_PATH")))
      (when ocaml-toplevel-path
        (expand-directory-name "../../share/emacs/site-lisp" ocaml-toplevel-path))))
  (ht-opam-config-env)
  (let ((opam-load-path (ht-opam-load-path)))
    (when opam-load-path
      (add-to-list 'load-path opam-load-path)))
  (use-package merlin
    :if (executable-find "ocamlmerlin")
    :commands merlin-mode
    :init
    (add-hook 'merlin-mode-hook 'company-mode)
    (add-hook 'tuareg-mode-hook 'merlin-mode)
    :config
    (add-to-list 'company-backends 'merlin-company-backend))
  (use-package utop
    :if (executable-find "utop"))
  (use-package utop-minor-mode
    :if (executable-find "utop")
    :init
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)))

(use-package undo-tree
  :ensure t
  :commands undo-tree-mode
  :diminish undo-tree-mode)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package whitespace
  :diminish whitespace-mode
  :init
  (setq whitespace-style '(face tabs lines-tail trailing empty)
        whitespace-line-column 80)
  (defun ht-style-whitespace-mode ()
    (set-face-attribute 'whitespace-line nil
                        :foreground nil
                        :background "gray90"))
  (add-hook 'whitespace-mode-hook 'ht-style-whitespace-mode))


;;; cosmetics

(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(show-paren-mode 1)

(setq frame-title-format
      '("" invocation-name ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(set-face-attribute 'mode-line          nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'header-line        nil :box nil)

(column-number-mode 1)

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

;;; truncate lines
(defun ht-truncate-lines ()
  (setq truncate-lines t))

(add-hook 'compilation-mode-hook     'ht-truncate-lines)
(add-hook 'dired-mode-hook           'ht-truncate-lines)
(add-hook 'prog-mode-hook            'ht-truncate-lines)
(add-hook 'shell-mode-hook           'ht-truncate-lines)
(add-hook 'sql-interactive-mode-hook 'ht-truncate-lines)

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
        (mplus-font (expand-file-name "mplus-1mn-regular.ttf" "~/Library/Fonts")))
    (setq explicit-shell-file-name ansi-term
          mac-command-modifier 'super
          mac-option-modifier 'meta)
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


;;; post init

(defun ht-elapsed-msg ()
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Load time: %.3fs"  elapsed)))

(add-hook 'after-init-hook 'ht-elapsed-msg)
