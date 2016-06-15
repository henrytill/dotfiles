;;; pre init

(defconst emacs-start-time (current-time))


;;; utility functions & macros

(eval-and-compile
  (defun is-darwin-p ()
    (string-equal system-type "darwin"))
  (defun is-linux-p ()
    (string-equal system-type "gnu/linux")))

(defun in-nix-shell-p ()
  (string-equal (getenv "IN_NIX_SHELL") "1"))

(defun expand-directory-name (dir &optional parent-dir)
  (file-name-as-directory (expand-file-name dir parent-dir)))

(defun ht-strip-trailing-newline (string)
  (replace-regexp-in-string "\n\\'" "" string))

(defmacro ht-comment (&rest body)
  "Comment out one or more s-expressions."
  nil)


;;; basic settings

(setq apropos-do-all t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      custom-file (expand-file-name "custom.el" user-emacs-directory)
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

(setq-default fill-column 80
              indent-tabs-mode nil
              ispell-program-name "aspell")

(load custom-file t)

(defconst ht-global-bindings
  '(("M-/"     . hippie-expand)
    ("C-x C-b" . ibuffer)
    ("C-s"     . isearch-forward-regexp)
    ("C-r"     . isearch-backward-regexp)
    ("C-M-s"   . isearch-forward)
    ("C-M-r"   . isearch-backward)
    ("M-%"     . query-replace-regexp)
    ("C-M-%"   . query-replace)))

(dolist (binding ht-global-bindings)
  (let ((key (car binding))
        (cmd (cdr binding)))
    (global-set-key (kbd key) cmd)))


;;; package.el

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/"))
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
    (cond ((is-darwin-p) (let ((oz-app-path "/Applications/Mozart2.app"))
                           (when (file-directory-p oz-app-path)
                             (concat oz-app-path "/Contents/Resources"))))
          ((is-linux-p)  (let ((oz-binary-path (executable-find "oz")))
                           (when oz-binary-path
                             (car (split-string oz-binary-path "/bin/oz")))))))
  (defun ht-oz-load-path ()
    (cond ((is-darwin-p) "~/src/other/mozart-elisp")
          ((is-linux-p)  (expand-directory-name "share/mozart/elisp" (ht-oz-home))))))


;;; packages

(require 'use-package)
(require 'bind-key)
(require 'diminish "diminish-0.44.el")

(use-package ace-window          :ensure t)
(use-package dash                :ensure t :defer t)
(use-package idle-highlight-mode :ensure t :defer t)
(use-package pandoc-mode         :ensure t)
(use-package pkg-info            :ensure t :defer t)
(use-package queue               :ensure t :defer t)
(use-package spinner             :ensure t)

(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :diminish paredit-mode)

(use-package agda2-mode
  :if (executable-find "agda-mode")
  :mode "\\.agda\\'"
  :defines agda2-include-dirs
  :init
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  (defun ht-agda-mode ()
    (let ((agda-includes (cons "." (split-string (getenv "buildDependsAgdaShareAgda")))))
      (dolist (var agda-includes)
        (add-to-list 'agda2-include-dirs var))))
  (add-hook 'agda2-mode-hook 'ht-agda-mode))

(use-package alert
  :ensure t
  :config
  (cond
   ((is-darwin-p) (setq-default alert-default-style 'ignore))
   ((is-linux-p)  (setq-default alert-default-style 'libnotify))))

(use-package auth-source
  :config
  (let ((authinfo (expand-file-name "authinfo.gpg" "~/Dropbox/doc")))
    (when (file-exists-p authinfo)
      (add-to-list 'auth-sources authinfo))))

(use-package avy
  :ensure t
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
  (add-hook 'cider-mode-hook      'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  :config
  (use-package cider-apropos)
  (use-package cider-classpath)
  (use-package cider-macroexpansion)
  (use-package cider-scratch)
  (use-package cider-selector)
  (setq cider-repl-display-help-banner nil
        cider-show-error-buffer 'except-in-repl))

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
  (defun ht-colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'ht-colorize-compilation-buffer))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode)

(use-package erc
  :defer t
  :config
  (setq erc-fill-function 'erc-fill-static
        erc-fill-static-center 19
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-keywords '("henrytill" "musnix")
        erc-prompt ">"
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (defun my-erc-hook (&optional match-type nick message)
    (let (alert-log-messages)
      (alert (or message (buffer-string))
             :title (concat "ERC: " (or nick (buffer-name)))
             :severity 'high
             :data message)))
  (add-hook 'erc-text-matched-hook 'my-erc-hook)
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
  :config
  (dolist (mode '(cider-repl-mode
                  cider-stacktrace-mode
                  geiser-repl-mode
                  haskell-interactive-mode
                  inferior-caml-mode
                  inferior-emacs-lisp-mode
                  inferior-forth-mode
                  inferior-python-mode
                  inferior-scheme-mode
                  inferior-sml-mode
                  prolog-inferior-mode
                  sbt-mode
                  shell-mode
                  slime-repl-mode
                  term-mode
                  tuareg-interactive-mode
                  utop-mode))
    (progn (when (member mode evil-insert-state-modes)
             (delete mode evil-insert-state-modes))
           (add-to-list 'evil-emacs-state-modes mode)))
  (defun ht-other-window ()
    (interactive)
    (other-window 1))
  (defconst evil-emacs-state-bindings
    '(("C-w C-w" . ht-other-window)
      ("C-w s"   . split-window-below)
      ("C-w v"   . split-window-right)
      ("C-w o"   . delete-other-windows)
      ("C-w c"   . delete-window)
      ("C-w q"   . ido-kill-buffer)
      ("C-o"     . evil-execute-in-normal-state)))
  (dolist (binding evil-emacs-state-bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (define-key evil-emacs-state-map (kbd key) cmd)))
  ;; paredit
  (evil-define-state paredit "Paredit state." :tag " <PAR> "
    :enable (paredit normal)
    :intercept-esc nil)
  (defconst evil-paredit-state-bindings
    '(("j"     . paredit-forward)
      ("k"     . paredit-backward)
      ("h"     . paredit-backward-up)
      ("l"     . paredit-forward-down)
      ("C-b"   . paredit-backward-down)
      ("C-f"   . paredit-forward-up)
      ("J"     . evil-next-line)
      ("K"     . evil-previous-line)
      ("H"     . evil-backward-char)
      ("L"     . evil-forward-char)
      ("M-r"   . paredit-raise-sexp)
      ("M-c"   . paredit-convolute-sexp)
      (")"     . paredit-forward-slurp-sexp)
      ("}"     . paredit-forward-barf-sexp)
      ("("     . paredit-backward-slurp-sexp)
      ("{"     . paredit-backward-barf-sexp)
      ("C-d"   . paredit-forward-delete)
      ("DEL"   . paredit-backward-delete)
      ("M-d"   . paredit-forward-kill-word)
      ("M-DEL" . paredit-backward-kill-word)
      ("M-j"   . paredit-splice-sexp-killing-forward)
      ("M-k"   . paredit-splice-sexp-killing-backward)
      ("C-o"   . evil-execute-in-normal-state)))
  (dolist (binding evil-paredit-state-bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (define-key evil-paredit-state-map (kbd key) cmd)))
  ;; leadering
  (use-package bind-map
    :ensure t
    :config
    (bind-map ht-base-leader-map
      :keys ("M-m")
      :evil-keys ("SPC")
      :evil-states (motion normal visual paredit))
    (bind-map-set-keys ht-base-leader-map
      "w" 'ace-window
      "x" 'smex
      "l" 'evil-paredit-state)
    ;; avy
    (bind-map ht-avy-leader-map
      :keys ("M-m g")
      :evil-keys ("SPC g"))
    (bind-map-set-keys ht-avy-leader-map
      ";" 'evil-avy-goto-char
      "'" 'evil-avy-goto-char-2
      "w" 'evil-avy-goto-word-1
      "l" 'evil-avy-goto-line)
    ;; ido
    (bind-map ht-ido-leader-map
      :keys ("M-m b")
      :evil-keys ("SPC b"))
    (bind-map-set-keys ht-ido-leader-map
      "b" 'ido-switch-buffer
      "f" 'ido-find-file
      "k" 'ido-kill-buffer)
    ;; magit
    (bind-map ht-magit-leader-map
      :keys ("M-m m")
      :evil-keys ("SPC m"))
    (bind-map-set-keys ht-magit-leader-map
      "s" 'magit-status))
  ;; surround
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))
  ;; enable
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
  :init
  (use-package flycheck-haskell
    :ensure t
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))
  :config
  (flycheck-define-checker racket-alt
    "A Racket syntax checker using the Racket compiler. See URL `http://racket-lang.org/'."
    :command ("racket" "-f" source-inplace)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
    :modes scheme-mode
    :predicate ht-rkt-predicate)
  (add-to-list 'flycheck-checkers 'racket-alt)
  (setq flycheck-completion-system 'ido)
  (setq-default flycheck-disabled-checkers '(javascript-jslint)))

(use-package forth-mode
  :if (executable-find "gforth")
  :mode "\\.fs\\'"
  :defines (forth-indent-level forth-minor-indent-level forth-hilight-level)
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
  (setq geiser-active-implementations '(racket)
        geiser-default-implementation 'racket))

(use-package gnus-desktop-notify
  :ensure t
  :commands gnus-desktop-notify-mode
  :preface
  (defun ht-gnus-desktop-notify ()
    (gnus-desktop-notify-mode)
    (setq gnus-desktop-notify-groups 'gnus-desktop-notify-explicit))
  (add-hook 'gnus-group-mode-hook 'ht-gnus-desktop-notify))

(use-package grep
  :config
  (add-to-list 'grep-find-ignored-directories "target"))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\(c\\|-boot\\)?\\'"
  :init
  (defun ht-haskell-mode ()
    (when (executable-find "hasktags")
      (setq haskell-tags-on-save t))
    (setq tab-width 4
          haskell-indentation-layout-offset 4
          haskell-indentation-left-offset 4))
  (add-hook 'haskell-mode-hook 'electric-pair-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'ht-haskell-mode))

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
  ;; Vertical Ido Results
  (setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
                          " [No match]" " [Matched]" " [Not readable]"
                          " [Too big]" " [Confirm]")
        ido-enable-flex-matching t)
  (add-hook 'ido-minibuffer-setup-hook
            (defun ido-disable-line-truncation ()
              (set (make-local-variable 'truncate-lines) nil)))
  (defun ht-ido-define-keys ()
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ht-ido-define-keys)
  (ido-mode t))

(use-package ielm
  :commands ielm
  :init
  (add-hook 'ielm-mode-hook 'company-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode))

(use-package js
  :init
  (add-hook 'js-mode-hook 'electric-pair-mode)
  :config
  (setq js-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter ("node" . js2-mode)
  :init
  (defun ht-npm-local-executable-path (name)
    (let* ((npm-bin-path (string-trim (shell-command-to-string "npm bin")))
           (npm-exe-path (expand-file-name name npm-bin-path)))
      (when (file-executable-p npm-exe-path) npm-exe-path)))
  (defun ht-js2-init ()
    (setq js2-additional-externs '("describe" "it"))
    (setq flycheck-javascript-eslint-executable (ht-npm-local-executable-path "eslint"))
    (flycheck-mode 1))
  (add-hook 'js2-init-hook 'ht-js2-init)
  :config
  (setq js2-basic-offset 2
        js2-include-node-externs t
        js2-indent-switch-body t))

(use-package lisp-mode
  :init
  (add-hook 'lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-to-list 'magic-mode-alist '("#! emacs --script" . emacs-lisp-mode)))

(use-package magit
  :ensure t
  :commands magit-status
  :config
  (put 'magit-clean 'disabled nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'whitespace-mode))

(use-package mmm-mode
  :ensure t
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-group 'html-js2
                 '((js-script-cdata
                    :submode js2-mode
                    :face mmm-code-submode-face
                    :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
                    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>")
                   (js-script
                    :submode js2-mode
                    :face mmm-code-submode-face
                    :front "<script[^>]*>[ \t]*\n?"
                    :back "[ \t]*</script>"
                    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">\n"
                                 @ "" _ "" @ "\n</script>" @)))))
  (mmm-add-mode-ext-class 'html-mode nil 'html-css)
  (mmm-add-mode-ext-class 'html-mode nil 'html-js2))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package org
  :ensure org-plus-contrib
  :bind (("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :functions org-bookmark-jump-unhide
  :init
  (use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook 'org-bullets-mode))
  (use-package cdlatex
    :ensure t
    :init
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex))
  (defun ht-prettify-org-mode ()
    (interactive)
    (let ((variable-faces '((org-agenda-structure . 2.0)
                            (org-document-title   . 1.5)
                            (org-level-1          . 1.75)
                            (org-level-2          . 1.5)
                            (org-level-3          . 1.25)
                            (org-level-4          . 1.1)
                            (org-level-5          . nil)
                            (org-level-6          . nil)
                            (org-level-7          . nil)
                            (org-level-8          . nil)))
          (fixed-faces     '(org-block
                             org-code
                             org-date
                             org-table
                             org-verbatim))
          (meta-faces      '(org-block-begin-line
                             org-block-end-line
                             org-document-info-keyword
                             org-meta-line)))
      (variable-pitch-mode t)
      (setq org-src-fontify-natively nil)
      (set-face-attribute 'org-link nil :font (plist-get ht-fixed-font :font))
      (dolist (face variable-faces)
        (if (cdr face)
            (set-face-attribute (car face) nil :height (cdr face) :inherit 'variable-pitch)
          (set-face-attribute (car face) nil :inherit 'variable-pitch)))
      (dolist (face fixed-faces)
        (set-face-attribute face nil :inherit 'fixed-pitch))
      (dolist (face meta-faces)
        (set-face-attribute face nil :inherit 'variable-pitch :foreground (face-foreground 'font-lock-comment-face nil)))))
  (add-hook 'org-mode-hook 'ht-prettify-org-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((clojure    . t)
                                                           (emacs-lisp . t)
                                                           (forth      . t)
                                                           (haskell    . t)
                                                           (js         . t)
                                                           (ocaml      . t)
                                                           (oz         . t)
                                                           (scala      . t)
                                                           (scheme     . t)
                                                           (shell      . t)))
  (eval-after-load 'ob-scheme (load-file (locate-file "ob-scheme-fix.el" load-path)))
  (setq org-babel-clojure-backend 'cider
        org-completion-use-ido t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-export-babel-evaluate nil
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window)
  (setq org-link-abbrev-alist
        '(("hoogle"         . "https://www.haskell.org/hoogle/?hoogle=")
          ("pinboard-topic" . "https://pinboard.in/u:henrytill/t:")))
  (when (file-directory-p org-directory)
    (let* ((notes-file (expand-file-name "notes.org" org-directory))
           (todo-file  (expand-file-name "todo.org"  org-directory))
           (notes-template
            `("n" "Notes" entry (file ,notes-file) "* %?\n  %i\n  %a"))
           (todo-template
            `("t" "Todo" entry (file+headline ,todo-file "Tasks") "* TODO %?\n  %i\n  %a")))
      (setq org-agenda-files (list org-directory)
            org-capture-templates (list notes-template todo-template)
            org-default-notes-file notes-file)
      (set-register ?n `(file . ,(expand-file-name "notes.org" org-directory))))))

(use-package oz
  :if (executable-find "oz")
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
  :config
  (projectile-global-mode))

(use-package proof-site
  :load-path "site-lisp/PG/generic"
  :init
  (use-package coq-mode
    :defer t
    :init
    (use-package company-coq
      :ensure t
      :init
      (add-hook 'coq-mode-hook 'company-coq-mode))
    (add-hook 'coq-mode-hook 'electric-pair-mode)
    (add-hook 'coq-mode-hook 'whitespace-mode))
  (with-eval-after-load 'proof-faces
    (set-face-attribute 'proof-locked-face nil :background "#222232")))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook 'electric-pair-mode))

(use-package saveplace
  :config
  (setq-default save-place t))

(use-package scala-mode
  :ensure t
  :init
  (use-package sbt-mode :ensure t)
  (defun ht-scala-mode ()
    (setq scala-indent:align-parameters t))
  (add-hook 'scala-mode-hook 'ht-scala-mode)
  (add-hook 'scala-mode-hook 'electric-pair-mode))

(use-package scheme
  :init
  (defun ht-scheme-mode ()
    (dolist (form+n '((conde . 0)
                      (fresh . 1)
                      (run   . 2)
                      (run*  . 1)))
      (put (car form+n) 'scheme-indent-function (cdr form+n))))
  (add-hook 'scheme-mode-hook 'ht-scheme-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  :config
  (when (executable-find "plt-r5rs")
    (setq scheme-program-name "plt-r5rs")))

(use-package shell
  :commands shell
  :init
  (defun ht-add-mode-line-dirtrack ()
    (add-to-list 'mode-line-buffer-identification
                 '(:propertize ("" default-directory "  ") face dired-directory)))
  (add-hook 'shell-mode-hook 'ht-add-mode-line-dirtrack))

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

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :init
  (use-package tide
    :ensure t
    :commands tide-setup)
  (defun ht-typescript-mode ()
    (tide-setup)
    (flycheck-mode 1)
    (eldoc-mode 1)
    (define-key evil-normal-state-local-map (kbd "C-]") 'tide-jump-to-definition)
    (define-key evil-normal-state-local-map (kbd "C-t") 'tide-jump-back))
  (add-hook 'typescript-mode-hook 'ht-typescript-mode))

(use-package tuareg
  :ensure t
  :mode (("\\.ml[ilpy]?\\'" . tuareg-mode)
         ("\\.eliomi?\\'"   . tuareg-mode))
  :init
  (defun ht-setup-tuareg ()
    (when (executable-find "opam")
      (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
        (setenv (car var) (cadr var))))
    (let ((ocaml-toplevel-path (getenv "OCAML_TOPLEVEL_PATH")))
      (when ocaml-toplevel-path
        (add-to-list 'load-path (expand-directory-name "../../share/emacs/site-lisp" ocaml-toplevel-path))))
    (when (in-nix-shell-p)
      (let ((merlin-site-lisp (getenv "MERLIN_SITE_LISP"))
            (utop-site-lisp   (getenv "UTOP_SITE_LISP"))
            (ocamlinit        (getenv "OCAMLINIT")))
        (when merlin-site-lisp
          (add-to-list 'load-path merlin-site-lisp))
        (when utop-site-lisp
          (add-to-list 'load-path utop-site-lisp))
        (when ocamlinit
          (setq tuareg-opam                nil
                org-babel-ocaml-command    (format "ocaml -init %s"       ocamlinit)
                tuareg-interactive-program (format "ocaml -init %s"       ocamlinit)
                utop-command               (format "utop -emacs -init %s" ocamlinit))))))
  (ht-setup-tuareg)
  (use-package merlin
    :if (executable-find "ocamlmerlin")
    :commands merlin-mode
    :defines merlin-command
    :init
    (add-hook 'merlin-mode-hook 'company-mode)
    (add-hook 'tuareg-mode-hook 'merlin-mode)
    :config
    (when (and (executable-find "opam")
               (not (in-nix-shell-p)))
      (setq merlin-command 'opam))
    (add-to-list 'company-backends 'merlin-company-backend))
  (use-package utop
    :if (executable-find "utop"))
  (use-package utop-minor-mode
    :if (executable-find "utop")
    :init
    (when (fboundp 'utop-minor-mode)
      (add-hook 'tuareg-mode-hook 'utop-minor-mode)))
  :config
  (setq tuareg-indent-align-with-first-arg nil))

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
        whitespace-line-column 100)
  (ht-comment
   (defun ht-style-whitespace-mode ()
     (set-face-attribute 'whitespace-line nil
                         :foreground nil
                         :background "gray90"))
   (add-hook 'whitespace-mode-hook 'ht-style-whitespace-mode)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (setq yas-prompt-functions '(yas-ido-prompt))
  (yas-global-mode 1))


;;; cosmetics

(setq frame-background-mode 'dark)

(defconst ht-fixed-font    '(:font "M+ 2m medium"))
(defconst ht-variable-font '(:font "M+ 2p medium"))

(defun ht-custom-set-faces ()
  (custom-set-faces `(fixed-pitch    ((t ,ht-fixed-font)))
                    `(variable-pitch ((t ,ht-variable-font)))))

(ht-custom-set-faces)

(defun ht-after-make-frame-function (frame)
  (with-selected-frame frame
    (load-theme 'inl t)))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'ht-after-make-frame-function)
  (load-theme 'inl t))

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

(blink-cursor-mode 0)
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
(when (is-darwin-p)
  (setq browse-url-browser-function 'browse-url-default-browser
        dired-use-ls-dired nil
        explicit-shell-file-name (expand-file-name "ansi-term" user-emacs-directory))
  (defun ht-dired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))
  (defadvice dired-readin (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (ht-dired-sort)))

(when (and (is-darwin-p) (window-system))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta)
  (when (member "M+ 1mn" (font-family-list))
    (set-face-attribute 'default nil :font "M+ 1mn 14"))
  (add-to-list 'default-frame-alist '(height . 40))
  (add-to-list 'default-frame-alist '(width . 100))
  (add-to-list 'default-frame-alist '(foreground-color . "#bbbbbb"))
  (add-to-list 'default-frame-alist '(background-color . "#222222"))
  (defun ht-reset-frame ()
    (interactive)
    (let ((height (cdr (assq 'height default-frame-alist)))
          (width  (cdr (assq 'width  default-frame-alist))))
      (set-frame-height (selected-frame) height)
      (set-frame-width  (selected-frame) width))))

;;; linux
(when (is-linux-p)
  (setq dired-listing-switches (concat dired-listing-switches " --group-directories-first")))

;;; nixos
(when (and (is-linux-p) (file-directory-p "/etc/nixos"))
  (require 'tramp)
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
  (setq browse-url-browser-function 'browse-url-chromium)
  (set-face-attribute 'region nil :background "lightgoldenrod2")
  (set-face-attribute 'region nil :foreground "black"))


;;; registers

(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))


;;; experiments

(defun ht-erc-sesh ()
  (interactive)
  (let ((erc-sesh (expand-file-name "erc-sesh.el.gpg" "~/Dropbox/doc")))
    (when (file-exists-p erc-sesh)
      (load-file erc-sesh))))


;;; post init

(defun ht-elapsed-msg ()
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Load time: %.3fs"  elapsed)))

(add-hook 'after-init-hook 'ht-elapsed-msg)
