;;; init.el

;; Reduce the frequency of garbage collection during startup
(setq gc-cons-threshold  (* 50 1000 1000))

;; Resets gc-cons-threshold to default
(defun ht/reset-gc-cons-threshold ()
  (custom-reevaluate-setting 'gc-cons-threshold))

(defun ht/emacs-ready-msg ()
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'ht/reset-gc-cons-threshold)
(add-hook 'emacs-startup-hook #'ht/emacs-ready-msg)

;;; PRELUDE ;;;

(defun is-darwin-p ()
  (string-equal system-type "darwin"))

(defun is-linux-p ()
  (string-equal system-type "gnu/linux"))

(defun is-windows-p ()
  (or (string-equal system-type "windows-nt")
      (string-equal system-type "cygwin")))

(defun in-nix-shell-p ()
  (string-equal (getenv "IN_NIX_SHELL") "1"))

(defun expand-directory-name (dir &optional parent-dir)
  (file-name-as-directory (expand-file-name dir parent-dir)))

(defmacro ht/comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(put 'ht/comment 'lisp-indent-function 'defun)

(defun ht/s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (save-match-data
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))

(defun ht/s-trim-right (s)
  "Remove whitespace at the end of S."
  (save-match-data
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s)))

(defun ht/s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (ht/s-trim-left (ht/s-trim-right s)))

(defun ht/replace-item-in-alist (alist key value)
  (cons `(,key . ,value) (assq-delete-all key alist)))

(defun ht/hostname ()
  (when (executable-find "hostname")
    (ht/s-trim (shell-command-to-string "hostname -s"))))

;; https://www.emacswiki.org/emacs/ElispCookbook
(defun ht/list-subdirs (dir exclude)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir   (directory-file-name dir))
        (files (directory-files dir nil nil t))
        (dirs  '()))
    (dolist (file files)
      (unless (member file (append '("." "..") exclude))
        (let ((file (concat (file-name-as-directory dir) file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file (ht/list-subdirs file exclude)) dirs))))))
    dirs))

(defun ht/hex-to-decimal (start end)
  (interactive "r")
  (let ((input (if (use-region-p)
                   (buffer-substring start end)
                 (string (char-after)))))
    (message (number-to-string (string-to-number input 16)))))

(setq apropos-do-all t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      eldoc-echo-area-use-multiline-p nil
      epa-armor t
      gnutls-min-prime-bits 1024
      inhibit-startup-message t
      initial-scratch-message nil
      ispell-program-name "aspell"
      load-prefer-newer t
      mail-envelope-from 'header
      mail-specify-envelope-from 't
      message-sendmail-envelope-from 'header
      mouse-yank-at-point t
      require-final-newline t
      ring-bell-function 'ignore
      save-interprogram-paste-before-kill t
      save-place-file (concat user-emacs-directory "places")
      scroll-conservatively 1
      send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      tags-revert-without-query 1
      visible-bell t
      x-select-enable-primary t
      x-select-enable-clipboard t)

(setq-default fill-column 80
              indent-tabs-mode nil)

(when (is-darwin-p)
  (setq browse-url-browser-function 'browse-url-default-browser
        dired-use-ls-dired nil))

(load custom-file t)

(put 'dired-find-alternate-file 'disabled nil)

(defconst ht/global-bindings
  '(("C-x C-b" . ibuffer)))

(dolist (binding ht/global-bindings)
  (let ((key (car binding))
        (cmd (cdr binding)))
    (global-set-key (kbd key) cmd)))

;; https://stackoverflow.com/questions/5147060/how-can-i-access-directory-local-variables-in-my-major-mode-hooks
(defun ht/run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(add-hook 'hack-local-variables-hook #'ht/run-local-vars-mode-hook)

;;; PACKAGES ;;;

(eval-and-compile
  (mapc #'(lambda (path)
            (push (expand-file-name path user-emacs-directory) load-path))
        '("site-lisp" "site-lisp/use-package"))
  (mapc #'(lambda (path)
            (when (file-directory-p path)
              (push (expand-file-name path) load-path)))
        '("/usr/local/share/emacs/site-lisp" "~/.nix-profile/share/emacs/site-lisp/")))

(require 'use-package)
(require 'bind-key)
(require 'package)

(dolist (archive '(("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (add-to-list 'package-archives archive))

(unless (eval-when-compile package--initialized)
  (package-initialize))

(when (null package-archive-contents)
  (package-refresh-contents))

(setq use-package-verbose t)

;;; COSMETICS ;;;

(use-package linum
  :if (version< emacs-version "26.1")
  :after (prog-mode)
  :hook (prog-mode . linum-on)
  :init
  (setq linum-format "%4d "))

(use-package display-line-numbers
  :if (version<= "26.1" emacs-version)
  :after (prog-mode)
  :hook (prog-mode . display-line-numbers-mode)
  :init
  (setq display-line-numbers-width 4))

(set-face-attribute 'font-lock-comment-face nil :foreground "#7f7f7f")

(setq frame-background-mode 'light)

(unless (and (is-linux-p) (window-system))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(when (and (is-windows-p) (window-system))
  (when (member "Consolas" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Consolas"
                        :foundry 'outline
                        :width 'normal
                        :height 98)))

(show-paren-mode 1)
(column-number-mode 1)

(blink-cursor-mode 0)
(setq visible-cursor nil)
(setq-default cursor-type 'box)

(defun ht/truncate-lines ()
  (setq truncate-lines t))

(dolist (mode-hook '(compilation-mode-hook
                     dired-mode-hook
                     prog-mode-hook
                     shell-mode-hook
                     sql-interactive-mode-hook))
  (add-hook mode-hook #'ht/truncate-lines))

;;; GREPPING ;;;

(use-package grep
  :config
  (ht/comment
    ;; Some experiments with ripgrep
    (defvar ht/rg-template "rg -nH --sort path --no-heading -e <R> -- <F>")
    (grep-apply-setting 'grep-command "rg -nH --sort path --no-heading -e")
    (grep-apply-setting 'grep-template ht/rg-template)
    nil))

;;; GENERAL ;;;

(use-package ace-window
  :ensure t
  :defer t)

(use-package undo-tree
  :ensure t
  :commands undo-tree-mode
  :init
  (setq undo-tree-auto-save-history nil))

(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package paredit
  :load-path "site-lisp/paredit"
  :commands enable-paredit-mode)

(use-package compile-commands
  :load-path "site-lisp/compile-commands"
  :commands compile-commands-get-include-directories)

;;; IVY ;;;

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
  (setq enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;;; EVIL-MODE ;;;

(defun ht/other-window ()
  (interactive)
  (other-window 1))

(defconst ht/evil-emacs-state-modes
  '(cider-repl-mode
    cider-stacktrace-mode
    geiser-repl-mode
    haskell-error-mode
    haskell-interactive-mode
    idris-repl-mode
    inferior-caml-mode
    inferior-emacs-lisp-mode
    inferior-forth-mode
    inferior-haskell-mode
    inferior-python-mode
    inferior-scheme-mode
    inferior-sml-mode
    ocamldebug-mode
    prolog-inferior-mode
    sbt-mode
    shell-mode
    term-mode
    tuareg-interactive-mode
    utop-mode
    xref--xref-buffer-mode))

(defun ht/setup-evil-emacs-state-modes ()
  (dolist (mode ht/evil-emacs-state-modes)
    (progn (when (member mode evil-insert-state-modes)
             (delete mode evil-insert-state-modes))
           (when (member mode evil-normal-state-modes)
             (delete mode evil-normal-state-modes))
           (add-to-list 'evil-emacs-state-modes mode))))

(defconst ht/evil-emacs-state-bindings
  '(("C-w C-w" . ht/other-window)
    ("C-w s"   . split-window-below)
    ("C-w v"   . split-window-right)
    ("C-w o"   . delete-other-windows)
    ("C-w c"   . delete-window)
    ("C-w q"   . kill-buffer)
    ("C-o"     . evil-execute-in-normal-state)))

(defconst ht/evil-normal-state-bindings
  '(("C-w C-]" . find-tag-other-window)
    ("g x"     . browse-url-at-point)))

(defun ht/setup-evil-bindings ()
  (dolist (binding ht/evil-emacs-state-bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (define-key evil-emacs-state-map (kbd key) cmd)))
  (dolist (binding ht/evil-normal-state-bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (define-key evil-normal-state-map (kbd key) cmd))))

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

(defun ht/setup-evil-paredit-state ()
  (evil-define-state paredit "Paredit state."
    :tag " <PAR> "
    :enable (paredit normal)
    :intercept-esc nil
    :entry-hook (enable-paredit-mode)
    :exit-hook (disable-paredit-mode))
  (dolist (binding evil-paredit-state-bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (define-key evil-paredit-state-map (kbd key) cmd))))

(defun ht/setup-evil-ex-commands ()
  (evil-define-command cfile     () (flymake-show-buffer-diagnostics))
  (evil-define-command tnext     () (find-tag nil t))
  (evil-define-command tprevious () (find-tag nil '-))
  (evil-ex-define-cmd "cf[ile]"     'cfile)
  (evil-ex-define-cmd "tn[ext]"     'tnext)
  (evil-ex-define-cmd "tp[revious]" 'tprevious))

(use-package hydra
  :ensure t)

(defvar ht/hydra-mode-specific nil "mode-specific hydra")

(defun ht/call-hydra-mode-specific ()
  (interactive)
  (if (boundp 'ht/hydra-mode-specific)
      (funcall ht/hydra-mode-specific)
    (message "no mode-specific hydra specified")))

(defhydra ht/hydra-base (:idle 1.0)
  "
base
----
_w_: ace-window           _a_: avy           _!_: shell-command
_x_: counsel-M-x          _g_: magit         _&_: async-shell-command
_b_: ivy-switch-buffer    _p_: project
_f_: counsel-find-file    _m_: mode-specific
_k_: kill-buffer          _i_: ibuffer
_l_: evil-paredit-state
"
  ("w" ace-window                  nil :exit t)
  ("x" counsel-M-x                 nil :exit t)
  ("b" ivy-switch-buffer           nil :exit t)
  ("f" counsel-find-file           nil :exit t)
  ("k" kill-buffer                 nil :exit t)
  ("l" evil-paredit-state          nil :exit t)
  ("a" ht/hydra-avy/body           nil :exit t)
  ("g" ht/hydra-magit/body         nil :exit t)
  ("p" ht/hydra-project/body       nil :exit t)
  ("m" ht/call-hydra-mode-specific nil :exit t)
  ("i" ibuffer                     nil :exit t)
  ("!" shell-command               nil :exit t)
  ("&" async-shell-command         nil :exit t))

(defhydra ht/hydra-project (:idle 1.0)
  "
project
-------
_p_: project-switch-project
_f_: project-find-file
_g_: project-find-regexp
_r_: project-query-replace-regexp
_d_: project-find-dir
_c_: project-compile
_s_: project-shell
_k_: project-kill-buffers
_t_: ht/project-generate-tags
"
  ("p" project-switch-project       nil :exit t)
  ("f" project-find-file            nil :exit t)
  ("g" project-find-regexp          nil :exit t)
  ("r" project-query-replace-regexp nil :exit t)
  ("d" project-find-dir             nil :exit t)
  ("c" project-compile              nil :exit t)
  ("s" project-shell                nil :exit t)
  ("k" project-kill-buffers         nil :exit t)
  ("t" ht/project-generate-tags     nil :exit t))

(use-package avy
  :ensure t
  :bind (:map isearch-mode-map ("C-c '" . avy-isearch))
  :commands (avy-goto-char
             avy-goto-char-2
             avy-goto-word-1
             avy-goto-line
             avy-isearch))

(defhydra ht/hydra-avy (:idle 1.0)
  "
avy
---
_;_: evil-avy-goto-char
_'_: evil-avy-goto-char-2
_w_: evil-avy-goto-word-1
_l_: evil-avy-goto-line
"
  (";" evil-avy-goto-char   nil :exit t)
  ("'" evil-avy-goto-char-2 nil :exit t)
  ("w" evil-avy-goto-word-1 nil :exit t)
  ("l" evil-avy-goto-line   nil :exit t))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "SPC") nil))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "SPC") nil))

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "SPC") nil)
  (define-key magit-mode-map (kbd "x") nil))

(defun ht/bind-xref-navigation-keys ()
  (bind-key "j" 'xref-next-line xref--xref-buffer-mode-map)
  (bind-key "k" 'xref-prev-line xref--xref-buffer-mode-map))

(add-hook 'xref--xref-buffer-mode-hook #'ht/bind-xref-navigation-keys)

(use-package evil
  :ensure t
  :config
  (setq evil-want-abbrev-expand-on-insert-exit nil
        evil-ex-search-case 'sensitive)
  (ht/setup-evil-emacs-state-modes)
  (ht/setup-evil-bindings)
  (ht/setup-evil-paredit-state)
  (ht/setup-evil-ex-commands)
  (define-key evil-normal-state-map (kbd "SPC") 'ht/hydra-base/body)
  (define-key evil-visual-state-map (kbd "SPC") 'ht/hydra-base/body)
  (define-key evil-emacs-state-map  (kbd "M-m") 'ht/hydra-base/body)
  (evil-mode 1))

;;; ALIGN ;;;

(with-eval-after-load 'align
  ;; alignment of haskell forms
  (nconc align-rules-list
         (mapcar (lambda (x)
                   `(,(car x) (regexp . ,(cdr x)) (modes quote (haskell-mode
                                                                literate-haskell-mode))))
                 '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (haskell-assignment  . "\\(\\s-+\\)=\\s-+")
                   (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+"))))
  ;; alignment of ocaml forms
  (nconc align-rules-list
         (mapcar (lambda (x)
                   `(,(car x) (regexp . ,(cdr x)) (modes quote (tuareg-mode))))
                 '((ocaml-types       . "\\(\\s-+\\):\\s-+")
                   (ocaml-assignment  . "\\(\\s-+\\)=\\s-+")
                   (ocaml-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (ocaml-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")))))

;;; COMPANY ;;;

(use-package company
  :ensure t
  :commands (company-mode global-company-mode)
  :hook (after-init-hook . global-company-mode)
  :config
  (setq company-backends (remove 'company-clang company-backends)
        company-global-modes '(not eshell-mode)))

;;; COMPILE ;;;

(use-package compile
  :commands compile
  :init
  (ht/comment
    ;; https://stackoverflow.com/questions/4556368/compiling-c-with-emacs-on-windows-system
    (add-to-list 'compilation-error-regexp-alist
                 '(msvc "^[ \t]*\\([A-Za-z0-9\\.][^(]*\\.\\(cpp\\|c\\|h\\|hpp\\)\\)(\\([0-9]+\\)) *: +\\(error\\|fatal error\\|warning\\) C[0-9]+:" 1 3))
    nil)
  ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  (require 'ansi-color)
  (defun ht/colorize-compilation-buffer ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'ht/colorize-compilation-buffer))

(bind-key "<f6>" 'recompile)

;;; DIRED ;;;

(defun ht/dired-pwd ()
  (interactive)
  (dired default-directory))

(define-key evil-normal-state-map (kbd "-") #'ht/dired-pwd)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "-") #'dired-up-directory))

;;; LSP ;;;

(when (version<= "26.1" emacs-version)
  (use-package eglot
    :ensure t
    :commands eglot
    :config
    (setq eglot-server-programs
          (ht/replace-item-in-alist eglot-server-programs 'rust-mode '("rust-analyzer")))))

;;; MAGIT ;;;

(use-package magit
  :ensure t
  :commands magit-status
  :config
  (put 'magit-clean 'disabled nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))

(defhydra ht/hydra-magit (:idle 1.0)
  "
magit
-----
_s_: magit-status
"
  ("s" magit-status nil :exit t))

;;; MISC EDITING MODES ;;;

(use-package capnp-mode
  :if (locate-file "capnp-mode.el" load-path)
  :mode "\\.capnp\\'"
  :init
  (defun ht/capnp-mode ()
    (setq-local tab-width 2))
  (add-hook 'capnp-mode-hook #'ht/capnp-mode))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\(?:\\..*\\)?\\'")

(defun ht/truncate-lines ()
  (interactive)
  (toggle-truncate-lines 1))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (dolist (mode '(ht/truncate-lines
                  ht/hide-lines-tail-display
                  whitespace-mode))
    (add-hook 'markdown-mode-hook mode)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'"  . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

;;; PROG-MODE ;;;

(use-package prog-mode
  :defer t
  :init
  (defun ht/add-watchwords ()
    (font-lock-add-keywords nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\)" 1 font-lock-warning-face t))))
  (add-hook 'prog-mode-hook #'ht/add-watchwords)
  (dolist (mode '(auto-revert-mode
                  electric-pair-local-mode
                  undo-tree-mode))
    (add-hook 'prog-mode-hook mode)))

;;; SHELL ;;;

(defun eshell/clear ()
  "Clear the Eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/rgrep (&rest args)
  "Use Emacs grep facility instead of calling external grep."
  (eshell-grep "rgrep" args t))

(defalias 'eshell/view 'view-file)

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
              (add-to-list 'eshell-visual-commands "bash"))))

(defun ht/shell ()
  (add-to-list 'mode-line-buffer-identification '("" default-directory "  ")))

(use-package shell
  :commands shell
  :init
  (add-hook 'shell-mode-hook #'ht/shell))

;;; TEX ;;;

(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil))

(with-eval-after-load 'tex
  (when (and (is-linux-p) (executable-find "mupdf-x11"))
    (add-to-list 'TeX-view-program-list
                 '("mupdf" ("mupdf-x11" (mode-io-correlate " -p %(outpage)") " %o")))
    (add-to-list 'TeX-view-program-selection
                 '(output-pdf "mupdf"))))

;;; WHITESPACE ;;;

(use-package whitespace
  :commands whitespace-mode

  :init
  (setq whitespace-style '(face trailing)
        whitespace-line-column 100)

  (defun ht/whitespace-mode ()
    (when (derived-mode-p 'prog-mode)
      (whitespace-mode 1)))

  (add-hook 'hack-local-variables-hook #'ht/whitespace-mode)

  :config
  (defun ht/toggle-tabs-display ()
    (interactive)
    (whitespace-mode -1)
    (if (memq 'tabs whitespace-style)
        (setq whitespace-style (remove 'tabs whitespace-style))
      (add-to-list 'whitespace-style 'tabs))
    (whitespace-mode 1))

  (defun ht/hide-lines-tail-display ()
    (interactive)
    (whitespace-mode -1)
    (if (memq 'lines-tail whitespace-style)
        (setq whitespace-style (remove 'lines-tail whitespace-style))
      nil)
    (whitespace-mode 1))

  (defun ht/toggle-lines-tail-display ()
    (interactive)
    (whitespace-mode -1)
    (if (memq 'lines-tail whitespace-style)
        (setq whitespace-style (remove 'lines-tail whitespace-style))
      (add-to-list 'whitespace-style 'lines-tail))
    (whitespace-mode 1))

  nil)

;;; ATS ;;;

(defun ht/ats-load-path ()
  (expand-directory-name "utils/emacs" (getenv "PATSHOME")))

(use-package ats2-mode
  :if (file-directory-p (ht/ats-load-path))
  :load-path (lambda () (list (ht/ats-load-path)))
  :mode (("\\.cats\\'" . c-mode)
         ("\\.dats\\'" . ats-mode)
         ("\\.hats\\'" . ats-mode)
         ("\\.sats\\'" . ats-mode)))

;;; C/C++ ;;;

(with-eval-after-load 'cc-styles
  (c-add-style "bsd-tabs" '("bsd" (indent-tabs-mode . t)))
  (c-add-style "hcpp"     '((indent-tabs-mode . nil)
                            (c-basic-offset . 4)
                            (c-offsets-alist (statement-block-intro . +)
                                             (knr-argdecl-intro . 0)
                                             (substatement-open . 0)
                                             (substatement-label . 0)
                                             (label . 0)
                                             (statement-cont . +)
                                             (innamespace . 0))))
  (add-to-list 'c-default-style '(c-mode   . "hcpp"))
  (add-to-list 'c-default-style '(c++-mode . "hcpp")))

(dolist (mode '(electric-pair-mode))
  (add-hook 'c-mode-hook   mode)
  (add-hook 'c++-mode-hook mode))

(setq path-to-ctags "ctags")

(defun ht/run-ctags (files)
  (let* ((args '("-e" "--langmap=c:.c.h" "--c-kinds=+fp" "-R"))
         (arg-string (mapconcat 'identity args " "))
         (cmd-string (format "%s %s %s" path-to-ctags arg-string files)))
    (message "Running: %s" cmd-string)
    (shell-command cmd-string)))

(defun ht/generate-tags (dir-name)
  "Generate TAGS file."
  (interactive "Ddirectory: ")
  (ht/run-ctags (directory-file-name dir-name)))

(defun ht/project-generate-tags ()
  "Generate TAGS file in the current project's root."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (includes (compile-commands-get-include-directories)))
    (ht/run-ctags (mapconcat 'identity includes " "))))

;;; CLANG-FORMAT ;;;

(when (is-windows-p)
  (let ((clang-format-path (or (getenv "CLANG_FORMAT_PATH")
                               (executable-find "clang-format"))))
    (when clang-format-path
      (let* ((clang-bin-path         (file-name-directory clang-format-path))
             (clang-format-load-path (expand-directory-name "../share/clang" clang-bin-path)))
        (push clang-format-load-path load-path)))))

(use-package clang-format
  :if (locate-file "clang-format.el" load-path)
  :commands (clang-format clang-format-region clang-format-buffer)
  :init
  (let ((clang-format-path (getenv "CLANG_FORMAT_PATH")))
    (when clang-format-path
      (setq clang-format-executable clang-format-path))))

;;; CMAKE ;;;

(when (is-windows-p)
  (let ((cmake-path (executable-find "cmake")))
    (when cmake-path
      (let* ((cmake-bin-path (file-name-directory cmake-path))
             (cmake-load-path (expand-directory-name "../share/emacs/site-lisp" cmake-bin-path)))
        (push cmake-load-path load-path)))))

(use-package cmake-mode
  :if (locate-file "cmake-mode.el" load-path)
  :mode (("\\.cmake\\'"       . cmake-mode)
         ("CMakeLists.txt\\'" . cmake-mode)))

;;; FORTH ;;;

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
  (add-hook 'forth-mode-hook #'ht/forth-mode))

(use-package forth-block-mode
  :if (executable-find "gforth")
  :mode "\\.fb\\'"
  :init
  (autoload 'forth-block-mode "gforth.el"))

;;; GO ;;;

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

;;; HASKELL ;;;

(defun ht/haskell-mode ()
  (setq electric-indent-local-mode 0
        evil-auto-indent nil
        haskell-doc-prettify-types nil
        haskell-interactive-popup-errors nil
        haskell-process-log t
        haskell-process-type 'cabal-new-repl
        haskell-stylish-on-save t
        haskell-tags-on-save t
        whitespace-line-column 120))

(defun ht/haskell-wrapper-function-nix ()
  (interactive)
  (setq haskell-process-wrapper-function
        (lambda (argv) (append (list "nix-shell" "--command" )
                               (list (mapconcat 'identity argv " "))))))

(defun ht/haskell-wrapper-function-identity ()
  (interactive)
  (setq haskell-process-wrapper-function 'identity))

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'"      . haskell-mode)
         ("\\.hs-boot\\'" . haskell-mode)
         ("\\.hsc\\'"     . haskell-mode)
         ("\\.lhs\\'"     . literate-haskell-mode)
         ("\\.cabal\\'"   . haskell-cabal-mode))
  :init
  (dolist (mode '(company-mode
                  electric-pair-mode
                  haskell-indentation-mode
                  ht/haskell-mode
                  interactive-haskell-mode))
    (add-hook 'haskell-mode-hook mode)))

;;; CLOJURE ;;;

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.edn\\'"  . clojure-mode)
         ("\\.boot\\'" . clojure-mode))
  :interpreter ("boot" . clojure-mode))

;;; SCHEME ;;;

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
  (add-hook 'scheme-mode-hook #'ht/scheme-mode))

;;; "LISP" (COMMON LISP & ELISP) ;;;

(when (executable-find "sbcl")
  (setq inferior-lisp-program "sbcl"))

(use-package lisp-mode
  :defer t
  :init
  (ht/comment
    (dolist (mode-hook '(lisp-mode-hook
                         emacs-lisp-mode-hook
                         lisp-interaction-mode-hook))
      (add-hook mode-hook 'eldoc-mode))
    nil))

(use-package sly
  :ensure t
  :mode (("\\.lisp\\'" . sly-mode))
  :commands sly)

;;; LUA ;;;

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :init
  (setq lua-indent-level 2))

;;; NIX ;;;

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;;; OCAML ;;;

(defhydra ht/hydra-ocaml (:idle 1)
  "
ocaml
-----
_a_ : tuareg-find-alternate-file
_t_ : merlin-type-enclosing
_gd_: merlin-locate
"
  ("a"  tuareg-find-alternate-file nil :exit t)
  ("t"  merlin-type-enclosing      nil :exit t)
  ("gd" merlin-locate              nil :exit t))

(defun ht/setup-tuareg ()
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
              tuareg-interactive-program (format "ocaml -init %s"       ocamlinit)
              utop-command               (format "utop -emacs -init %s" ocamlinit))))))

(defun ht/merlin-mode ()
  (let ((extension (file-name-extension buffer-file-name)))
    (when (not (or (string-equal "mll" extension)
                   (string-equal "mly" extension)))
      (evil-define-key 'normal merlin-mode-map "gd" 'merlin-locate)
      (define-key evil-normal-state-local-map (kbd "C-]") 'merlin-locate)
      (define-key evil-normal-state-local-map (kbd "C-t") 'merlin-pop-stack)
      (merlin-mode 1)
      (company-mode 1)
      (when (and (executable-find "opam")
                 (not (in-nix-shell-p)))
        (setq merlin-command 'opam))
      (add-to-list 'company-backends 'merlin-company-backend))))

(defun ht/tuareg-set-compile-command ()
  (let ((build-dir (and (locate-dominating-file buffer-file-name "build")
                        (locate-dominating-file buffer-file-name "_tags"))))
    (when build-dir
      (setq default-directory build-dir)
      (set (make-local-variable 'compile-command) "./build"))))

(defun ht/tuareg-mode ()
  (setq-local ht/hydra-mode-specific 'ht/hydra-ocaml/body)
  (electric-indent-local-mode 0)
  (setq evil-auto-indent nil))

(with-eval-after-load 'caml-types
  (let ((color (face-attribute 'default :background)))
    (dolist (face '(caml-types-expr-face
                    caml-types-occ-face
                    caml-types-scope-face
                    caml-types-typed-face))
      (set-face-foreground face color))))

(with-eval-after-load 'caml-help
  (set-face-foreground 'ocaml-help-face (face-attribute 'default :background)))

(use-package tuareg
  :ensure t
  :mode (("\\.ml[ilpy]?\\'" . tuareg-mode)
         ("\\.eliomi?\\'"   . tuareg-mode))
  :init
  (ht/setup-tuareg)
  (use-package merlin
    :if (and (executable-find "ocamlmerlin")
             (locate-file "merlin.el" load-path))
    :commands merlin-mode
    :defines merlin-command
    :hook ((caml-mode-hook   . ht/merlin-mode)
           (tuareg-mode-hook . ht/merlin-mode)))
  (use-package utop
    :if (and (executable-find "utop")
             (locate-file "utop.el" load-path))
    :commands (utop utop-minor-mode)
    :hook (tuareg-mode-hook . utop-minor-mode))
  (use-package ocp-indent
    :if (and (executable-find "ocp-indent")
             (locate-file "ocp-indent.el" load-path))
    :commands ocp-setup-indent
    :hook (tuareg-mode-hook . ocp-setup-indent))
  (dolist (mode '(electric-pair-mode
                  ht/tuareg-mode))
    (add-hook 'tuareg-mode-hook mode)))

;;; OZ ;;;

(eval-and-compile
  (defun ht/oz-home ()
    (cond ((is-darwin-p) (let ((oz-app-path "/Applications/Mozart2.app"))
                           (when (file-directory-p oz-app-path)
                             (concat oz-app-path "/Contents/Resources"))))
          ((is-linux-p)  (let ((oz-binary-path (executable-find "oz")))
                           (when oz-binary-path
                             (car (split-string oz-binary-path "/bin/oz")))))))
  (defun ht/oz-load-path ()
    (cond ((is-darwin-p) "~/src/other/mozart-elisp")
          ((is-linux-p)  (expand-directory-name "share/mozart/elisp" (ht/oz-home))))))

(use-package oz
  :disabled t
  :if (executable-find "oz")
  :load-path (lambda () (list (ht/oz-load-path)))
  :mode ("\\.oz\\'" . oz-mode)
  :commands run-oz
  :init
  (setenv "OZHOME" (ht/oz-home))
  (ht/comment
    (add-hook 'oz-mode-hook 'electric-pair-mode)
    (add-hook 'oz-mode-hook 'undo-tree-mode)
    (add-hook 'oz-mode-hook 'whitespace-mode)
    nil))

;;; RUST ;;;

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (dolist (mode '(electric-pair-mode))
    (add-hook 'rust-mode-hook mode)))

;;; SCALA ;;;

(defun ht/scala-mode ()
  (setq scala-font-lock:var-face 'font-lock-variable-name-face
        scala-indent:align-parameters t))

(use-package scala-mode
  :ensure t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'"   . scala-mode))
  :init
  (dolist (mode '(ht/scala-mode
                  electric-pair-mode))
    (add-hook 'scala-mode-hook mode)))

;;; SML ;;;

(use-package sml-mode
  :ensure t
  :mode "\\.sml\\'")

;;; JAVASCRIPT/TYPESCRIPT ;;;

(defun ht/npm-local-executable-path (name)
  (let* ((npm-bin-path (string-trim (shell-command-to-string "npm bin")))
         (npm-exe-path (expand-file-name name npm-bin-path)))
    (when (file-executable-p npm-exe-path) npm-exe-path)))

(use-package js
  :mode (("\\.json\\'" . js-mode)
         ("\\.js\\'"   . js-mode)
         ("\\.jsx\\'"  . js-mode))
  :init
  (add-hook 'js-mode-hook 'electric-pair-mode)
  :config
  (setq js-indent-level 4))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'")

(use-package web-mode
  :ensure t
  :mode "\\.tsx\\'")

;;; POSTLUDE ;;;

(with-eval-after-load 'tramp-sh
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

(when (and (is-darwin-p) (window-system))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

(when (string-equal "thaumas" (ht/hostname))
  (setq doc-view-resolution 150
        doc-view-scale-internally nil))

(when (is-windows-p)
  (let ((home (directory-file-name (getenv "USERPROFILE"))))
    (when home
      (setq default-directory home))))

;;; REGISTERS ;;;

(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))
