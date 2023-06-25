;;; -*- whitespace-line-column: 100; -*-

(message "Loading %s ..." load-file-name)

;;; Reduce the frequency of garbage collection during startup
(setq gc-cons-threshold  (* 50 1000 1000))

(defun ht/reset-gc-cons-threshold ()
  "Resets gc-cons-threshold to its default setting."
  (custom-reevaluate-setting 'gc-cons-threshold))

(defun ht/emacs-ready-msg ()
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'ht/reset-gc-cons-threshold)
(add-hook 'emacs-startup-hook #'ht/emacs-ready-msg)


;;; --- PRELUDE --- ;;;

(defun is-darwin-p ()
  (string-equal system-type "darwin"))

(defun is-linux-p ()
  (string-equal system-type "gnu/linux"))

(defun is-bsd-p ()
  (string-equal system-type "berkeley-unix"))

(defun is-unix-p ()
  (or (is-linux-p) (is-bsd-p)))

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

(defun ht/assq-replace (alist pair)
  (cons pair (assq-delete-all (car pair) alist)))

(defun ht/rassq-replace (alist value pair)
  (cons pair (rassq-delete-all value alist)))

(defun ht/hostname ()
  (when (executable-find "hostname")
    (ht/s-trim (shell-command-to-string "hostname -s"))))

(defun ht/hex-to-decimal (start end)
  (interactive "r")
  (let ((input (if (use-region-p)
                   (buffer-substring start end)
                 (string (char-after)))))
    (message (number-to-string (string-to-number input 16)))))

(if (version< emacs-version "27.1")
    (progn
      (require 'ido)
      (ido-mode t)
      (setq ido-default-buffer-method 'selected-window
            ido-default-file-method 'selected-window
            ido-enable-flex-matching t
            ido-use-filename-at-point t
            ido-use-virtual-buffers t))
  (progn
    (require 'icomplete)
    (fido-mode 1)
    (fido-vertical-mode 1)))

(require 'uniquify)

(setq apropos-do-all t
      backup-by-copying t
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
      uniquify-buffer-name-style 'forward
      visible-bell t
      x-select-enable-primary t
      x-select-enable-clipboard t)

(setq-default indent-tabs-mode nil)

(load custom-file t)

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(defconst ht/global-bindings
  '(("C-x C-b" . ibuffer)
    ("C-M-s" . isearch-forward)
    ("C-M-r" . isearch-backward)
    ("M-/" . hippie-expand)
    ("M-z" . zap-up-to-char)
    ("C-s" . isearch-forward-regexp)
    ("C-r" . isearch-backward-regexp)))

(dolist (binding ht/global-bindings)
  (let ((key (car binding))
        (cmd (cdr binding)))
    (global-set-key (kbd key) cmd)))

;;; https://stackoverflow.com/questions/5147060/how-can-i-access-directory-local-variables-in-my-major-mode-hooks
(defun ht/run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(add-hook 'hack-local-variables-hook #'ht/run-local-vars-mode-hook)


;;; --- LOAD-PATH & PACKAGES --- ;;;

(let ((directory (expand-file-name "/usr/share/emacs/site-lisp")))
  (when (file-directory-p directory)
    (let ((default-directory directory))
      (when (not (member default-directory load-path))
        (push default-directory load-path)
        (normal-top-level-add-subdirs-to-load-path)))))

(defconst ht/site-lisp-directory (expand-file-name "site-lisp" user-emacs-directory))

(mapc #'(lambda (path)
          (push (expand-file-name path ht/site-lisp-directory) load-path))
      '("." "use-package"))

(mapc #'(lambda (path)
          (when (file-directory-p path)
            (push (expand-file-name path) load-path)))
      '("/usr/local/share/emacs/site-lisp" "~/.nix-profile/share/emacs/site-lisp/"))

(require 'use-package)
(require 'bind-key)
(require 'package)

(dolist (archive '(("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (add-to-list 'package-archives archive))

(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(setq use-package-verbose t)


;;; --- COSMETICS --- ;;;

;;; https://codeberg.org/dnkl/foot/issues/549#issuecomment-201932
;;; https://codeberg.org/dnkl/foot/wiki#only-8-colors-in-emacs
(add-to-list 'term-file-aliases '("foot" . "xterm"))

(setq-default display-line-numbers-width 4
              display-line-numbers-widen t)

(dolist (mode-hook '(prog-mode-hook
                     conf-mode-hook
                     org-mode-hook))
  (add-hook mode-hook #'display-line-numbers-mode))

(setq frame-background-mode 'light)

(defun ht/set-font-lock-face-attributes ()
  (let ((comment-color "#2f4f4f")
        (region-color "#eeee9e"))
    (set-face-attribute 'font-lock-comment-face nil :foreground comment-color)
    (set-face-attribute 'font-lock-doc-face nil :foreground comment-color)
    (set-face-attribute 'region nil :background region-color)))

(add-hook 'font-lock-mode-hook #'ht/set-font-lock-face-attributes)

(when (display-graphic-p)
  (defconst ht/preferred-unix-font "PragmataPro Mono:size=12")
  (defconst ht/preferred-win-font "PragmataPro Mono:size=12"))

(defun ht/set-face-attributes (frame)
  (when (display-graphic-p)
    (set-face-attribute 'mode-line frame :box nil)
    (set-face-attribute 'mode-line-inactive frame :box nil)
    (cond ((is-unix-p)
           (progn (set-fontset-font "fontset-default" 'unicode ht/preferred-unix-font)
                  (set-face-attribute 'default frame :font ht/preferred-unix-font)))
          ((is-windows-p)
           (progn (set-fontset-font "fontset-default" 'unicode ht/preferred-win-font)
                  (set-face-attribute 'default frame :font ht/preferred-win-font))))))

(defun ht/remove-decorations ()
  (when (is-unix-p)
    (menu-bar-mode -1))
  (when (display-graphic-p)
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
    (tool-bar-mode -1)))

(defun ht/fix-split-behavior ()
  (when (and (is-unix-p) (display-graphic-p))
    (setq split-height-threshold nil)))

(defun ht/update-frame (frame)
  (select-frame frame)
  (ht/set-face-attributes frame)
  (ht/remove-decorations)
  (ht/fix-split-behavior))

(if (daemonp)
    (add-to-list 'after-make-frame-functions #'ht/update-frame)
  (ht/update-frame (selected-frame)))

(show-paren-mode 1)
(column-number-mode 1)

(blink-cursor-mode 0)
(setq visible-cursor nil)

(defun ht/truncate-lines ()
  (toggle-truncate-lines t))

(dolist (mode-hook '(bibtex-mode-hook
                     conf-mode-hook
                     dired-mode-hook
                     prog-mode-hook
                     sql-interactive-mode-hook
                     text-mode-hook))
  (add-hook mode-hook #'ht/truncate-lines))

(winner-mode 1)

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setq display-time-day-and-date t
      display-time-default-load-average nil)

(display-time-mode 1)


;;; --- GENERAL --- ;;;

;;; NAVIGATION

(defun ht/next-page ()
  (interactive)
  (narrow-to-page 1))

(defun ht/prev-page ()
  (interactive)
  (narrow-to-page -1)
  (beginning-of-buffer))

(bind-key "C-c n n" #'ht/next-page)
(bind-key "C-c n p" #'ht/prev-page)

;;; EDITING

(defun ht/move-line-up ()
  "Move up the current line or region"
  (interactive)
  (transpose-lines 1)
  (previous-line 2)
  (indent-according-to-mode))

(defun ht/move-line-down ()
  "Move down the current line or region"
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1)
  (indent-according-to-mode))

(bind-key "M-<up>" #'ht/move-line-up)
(bind-key "M-<down>" #'ht/move-line-down)

;;; SITE-LISP

(add-to-list 'load-path (expand-file-name "compile-commands" ht/site-lisp-directory))
(autoload 'compile-commands-get-include-directories "compile-commands.el")

(add-to-list 'load-path (expand-file-name "magit-annex" ht/site-lisp-directory))
(autoload 'magit-annex-dispatch "magit-annex.el")

(add-to-list 'load-path (expand-file-name "docker-tramp" ht/site-lisp-directory))
(autoload 'docker-tramp-add-method "docker-tramp.el")

(add-to-list 'load-path (expand-file-name "meson-mode" ht/site-lisp-directory))
(autoload 'meson-mode "meson-mode.el")

(add-to-list 'load-path (expand-file-name "nim-mode" ht/site-lisp-directory))

;;; XDG

(autoload 'xdg-data-home "xdg.el")

;;; IBUFFER

(setq ibuffer-default-sorting-mode 'filename/process)

;;; TRAMP

(with-eval-after-load 'tramp
  (docker-tramp-add-method)
  (tramp-set-completion-function docker-tramp-method docker-tramp-completion-function-alist))

;;; DIRED

(with-eval-after-load 'dired
  (require 'browse-url)
  (when (executable-find "zathura")
    (defun ht/browse-pdf-with-zathura (url &optional new-window)
      (start-process (concat "zathura \"" url "\"") nil "zathura" url))
    (add-to-list 'browse-url-handlers '("\\.pdf\\'" . ht/browse-pdf-with-zathura)))
  nil)

;;; INFO

(when (and (is-linux-p) (fboundp 'xdg-data-home))
  (require 'info)
  (add-to-list 'Info-additional-directory-list (expand-file-name "info" (xdg-data-home))))

;;; ORG-MODE

(with-eval-after-load 'org
  (message "Loading org config...")
  (require 'oc-csl)
  (add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "zathura -P %1 %s"))
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (shell . t)))
  nil)

;;; ALIGN

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

;;; PAREDIT

(use-package paredit
  :ensure t
  :commands enable-paredit-mode)

;;; COMPANY

(use-package company
  :ensure t
  :commands company-mode
  :hook ((prog-mode . company-mode)
         (shell-mode . company-mode)
         (org-mode . company-mode))
  :config
  (delete 'company-clang company-backends)
  (delete 'company-xcode company-backends))

;;; LSP

(use-package eglot
  :ensure t
  :commands eglot
  :config
  (setq eglot-server-programs
        (ht/assq-replace eglot-server-programs '(rust-mode . "rust-analyzer"))))

;;; MAGIT

(use-package magit
  :ensure t
  :commands (magit-status magit-project-status)
  :hook ((magit-status-mode . font-lock-mode)
         (magit-diff-mode . font-lock-mode))
  :config
  (put 'magit-clean 'disabled nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (bind-key "@" #'magit-annex-dispatch magit-mode-map))

;;; PROJECT

(with-eval-after-load 'project
  (when (and (boundp 'project-prefix-map)
             (boundp 'project-switch-commands))
    ;; Delete less useful commands
    (delete '(project-vc-dir "VC-Dir") project-switch-commands)
    (delete '(project-eshell "Eshell") project-switch-commands)
    ;; Add Magit
    (bind-key "m" #'magit-project-status project-prefix-map)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
    ;; Add Shell
    (bind-key "s" #'project-shell project-prefix-map)
    (add-to-list 'project-switch-commands '(project-shell "Shell") t)))

(defvar ht/last-project nil)

(defun ht/check-project ()
  (let ((current-project (project-root (project-current t))))
    (if (not (string-equal ht/last-project current-project))
        (setq ht/last-project current-project)
      nil)))

;;; PROG-MODE

(dolist (f '(auto-revert-mode
             electric-pair-local-mode))
  (add-hook 'prog-mode-hook f))

;;; WHITESPACE

(with-eval-after-load 'whitespace
  (setq whitespace-style '(face lines-tail trailing)
        whitespace-line-column 100))

(add-hook 'prog-mode-hook #'whitespace-mode)

(defun ht/whitespace-mode ()
  (when (and font-lock-mode (derived-mode-p 'prog-mode))
    (whitespace-mode 1)))

(add-hook 'hack-local-variables-hook #'ht/whitespace-mode)

(defun ht/toggle-tabs-display ()
  (interactive)
  (whitespace-mode -1)
  (let ((tab-mode (if font-lock-mode 'tabs 'tab-mark)))
    (if (memq tab-mode whitespace-style)
        (setq whitespace-style (remove tab-mode whitespace-style))
      (add-to-list 'whitespace-style tab-mode)))
  (whitespace-mode 1))

(defun ht/hide-lines-tail-display ()
  (interactive)
  (whitespace-mode -1)
  (when (memq 'lines-tail whitespace-style)
    (setq whitespace-style (remove 'lines-tail whitespace-style)))
  (whitespace-mode 1))

(defun ht/toggle-lines-tail-display ()
  (interactive)
  (whitespace-mode -1)
  (if (memq 'lines-tail whitespace-style)
      (setq whitespace-style (remove 'lines-tail whitespace-style))
    (add-to-list 'whitespace-style 'lines-tail))
  (whitespace-mode 1))

;;; TEXT

(add-hook 'text-mode-hook 'auto-revert-mode)


;;; --- PROGRAMMING LANGUAGES --- ;;;

;;; C/C++

(defun ht/modify-c-syntax-entries ()
  (modify-syntax-entry ?_ "w"))

(add-hook 'c-mode-common-hook #'ht/modify-c-syntax-entries)

(with-eval-after-load 'cc-mode
  ;; https://www.kernel.org/doc/html/v4.10/process/coding-style.html#you-ve-made-a-mess-of-it
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (save-excursion
      c-basic-offset))
  (c-add-style "ht" '("linux"
                      (indent-tabs-mode . t)
                      (c-offsets-alist (arglist-cont-nonempty c-lineup-arglist-tabs-only))))
  (add-to-list 'c-default-style '(c-mode . "ht")))

(setq path-to-ctags "ctags")

(defun ht/run-ctags (paths)
  (let* ((args '("-e" "--langmap=c:.c.h" "--c-kinds=+fp" "-R"))
         (arg-string (mapconcat 'identity args " "))
         (paths-string (mapconcat 'identity paths " "))
         (cmd-string (format "%s %s %s" path-to-ctags arg-string paths-string)))
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
    (ht/run-ctags includes)))

(defun ht/set-tab-width (width)
  (interactive "nSet tab-width to: ")
  (setq tab-width width))

(when (is-windows-p)
  (let ((clang-format-path (or (getenv "CLANG_FORMAT_PATH")
                               (executable-find "clang-format"))))
    (when clang-format-path
      (let* ((clang-bin-path (file-name-directory clang-format-path))
             (clang-format-load-path (expand-directory-name "../share/clang" clang-bin-path)))
        (push clang-format-load-path load-path)))))

(use-package clang-format
  :if (locate-file "clang-format.el" load-path)
  :commands (clang-format clang-format-region clang-format-buffer)
  :config
  (let ((clang-format-path (getenv "CLANG_FORMAT_PATH")))
    (when clang-format-path
      (setq clang-format-executable clang-format-path))))

(use-package bison-mode
  :ensure t
  :commands bison-mode
  :config
  (setq bison-rule-separator-column 4
        bison-rule-enumeration-column 4
        bison-decl-type-column 0
        bison-decl-token-column 0))

(when (is-windows-p)
  (let ((cmake-path (executable-find "cmake")))
    (when cmake-path
      (let* ((cmake-bin-path (file-name-directory cmake-path))
             (cmake-load-path (expand-directory-name "../share/emacs/site-lisp" cmake-bin-path)))
        (push cmake-load-path load-path)))))

(use-package cmake-mode
  :if (locate-file "cmake-mode.el" load-path)
  :commands cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(add-to-list 'auto-mode-alist '("/meson\\(\\.build\\|_options\\.txt\\)\\'" . meson-mode))

(use-package ninja-mode
  :if (locate-file "ninja-mode.el" load-path)
  :commands ninja-mode)

;;; APL

(use-package gnu-apl-mode
  :ensure t
  :commands (gnu-apl gnu-apl-mode)
  :init
  (setq gnu-apl-show-keymap-on-startup nil))

;;; FORTH

(use-package forth-mode
  :ensure t
  :commands (forth-mode run-forth))

(use-package forth-block-mode
  :after (forth-mode))

;;; GO

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

;;; HASKELL

(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :hook ((haskell-mode . haskell-indentation-mode)
         (haskell-mode . interactive-haskell-mode))
  :config
  (setq haskell-doc-prettify-types nil
        haskell-interactive-popup-errors nil
        haskell-process-log t
        haskell-process-type 'cabal-repl
        haskell-stylish-on-save t
        haskell-tags-on-save t))

;;; ELISP

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(use-package macrostep
  :ensure t
  :commands macrostep-expand)

(bind-key "C-c e" #'macrostep-expand emacs-lisp-mode-map)

;;; SCHEME

(defun ht/add-minikanren-indents ()
  (dolist (form+n '((conde . 0)
                    (fresh . 1)
                    (run . 2)
                    (run* . 1)))
    (put (car form+n) 'scheme-indent-function (cdr form+n))))

(dolist (f '(ht/add-minikanren-indents
             enable-paredit-mode))
  (add-hook 'scheme-mode-hook f))

(use-package geiser-guile
  :ensure t
  :defer t)

(use-package geiser-racket
  :ensure t
  :defer t)

(add-hook 'geiser-repl-mode-hook #'company-mode)

;;; COMMON LISP

(add-hook 'lisp-mode-hook #'enable-paredit-mode)

(when (executable-find "sbcl")
  (setq inferior-lisp-program "sbcl"))

(use-package sly
  :ensure t
  :commands sly)

(defun sly-common-lisp-indent-function (indent-point state)
  (common-lisp-indent-function indent-point state))

;;; JAVASCRIPT

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))

(with-eval-after-load 'js
  (setq js-indent-level 2))

;;; LUA

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config
  (setq lua-indent-level 4))

;;; NIM

(use-package nim-mode
  :if (locate-file "nim-mode.el" load-path)
  :mode "\\.nim\\'")

;;; NIX

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;;; OCAML

(defun ht/import-ocaml-env ()
  (when (executable-find "opam")
    (let ((default-directory (project-root (project-current t))))
      (make-local-variable 'process-environment)
      (setq process-environment (copy-sequence process-environment))
      (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
        (setenv (car var) (cadr var))))))

(defun ht/add-ocaml-load-path ()
  (if-let* ((ocaml-toplevel-path (getenv "OCAML_TOPLEVEL_PATH"))
            (ocaml-load-path (expand-directory-name "../../share/emacs/site-lisp" ocaml-toplevel-path)))
      (progn
        (make-local-variable 'load-path)
        (setq load-path (copy-sequence load-path))
        (add-to-list 'load-path ocaml-load-path))
    nil))

(defun ht/dune-project-exists-p ()
  (let* ((project-dir (project-root (project-current t)))
         (dune-project-file (expand-file-name "dune-project" project-dir)))
    (if (file-exists-p dune-project-file)
        dune-project-file
      nil)))

(defun ht/set-utop-command ()
  (when (executable-find "opam")
    (let* ((dune-cmd "dune utop . -- -emacs")
           (cmd "utop -emacs")
           (command (if (ht/dune-project-exists-p) dune-cmd cmd)))
      (setq utop-command (format "opam config exec -- %s" command)))))

(defun ht/load-ocaml-packages ()
  (when (ht/check-project)
    (dolist (feature '(merlin-mode
                       merlin-company
                       utop
                       utop-minor-mode
                       ocp-indent))
      (when (featurep feature)
        (unload-feature feature))))
  (when (locate-file "merlin.el" load-path)
    (when (not (featurep 'merlin-mode))
      (load "merlin.el")
      (setq merlin-command 'opam))
    (merlin-mode 1))
  (when (locate-file "merlin-company.el" load-path)
    (when (not (featurep 'merlin-company))
      (require 'merlin-company "merlin-company.el")))
  (when (locate-file "utop.el" load-path)
    (when (not (and (featurep 'utop) (featurep 'utop-minor-mode)))
      (require 'utop "utop.el")
      (require 'utop-minor-mode "utop.el")
      (ht/set-utop-command))
    (utop-minor-mode 1))
  (when (locate-file "ocp-indent.el" load-path)
    (when (not (featurep 'ocp-indent))
      (require 'ocp-indent "ocp-indent.el")
      (when (version<= "28.1" emacs-version)
        (defun ocp-indent-buffer ()
          (interactive nil)
          (ocp-indent-region 1 (buffer-size)))))
    (ocp-setup-indent))
  t)

(defun ht/load-ocaml-buffer ()
  (ht/import-ocaml-env)
  (when-let (ocaml-load-path (ht/add-ocaml-load-path))
    (ht/load-ocaml-packages)))

(use-package tuareg
  :ensure t
  :commands (tuareg-mode tuareg-menhir-mode tuareg-opam-mode)
  :hook ((tuareg-mode . electric-indent-local-mode)
         (tuareg-mode . ht/load-ocaml-buffer)))

(defvar ht/dune-fmt-command "dune fmt")

(defun ht/project-dune-fmt ()
  (interactive)
  (when (and (derived-mode-p 'tuareg-mode)
             (ht/dune-project-exists-p)
             (assoc 'auto-revert-mode minor-mode-alist))
    (let ((default-directory (project-root (project-current t)))
          (out-buffer (get-buffer-create "*dune-fmt-out*"))
          (err-buffer (get-buffer-create "*dune-fmt-err*")))
      (shell-command ht/dune-fmt-command out-buffer err-buffer))))

(use-package dune
  :if (and (executable-find "dune")
           (locate-file "dune.el" load-path))
  :mode (("dune\\'" . dune-mode)
         ("dune-project\\'" . dune-mode))
  :commands dune-mode)

(with-eval-after-load 'caml-types
  (let ((color (face-attribute 'default :background)))
    (dolist (face '(caml-types-expr-face
                    caml-types-occ-face
                    caml-types-scope-face
                    caml-types-typed-face))
      (set-face-foreground face color))))

(with-eval-after-load 'caml-help
  (set-face-foreground 'ocaml-help-face (face-attribute 'default :background)))

;;; PROLOG

(setq auto-mode-alist
      (ht/rassq-replace auto-mode-alist 'perl-mode '("\\.pl\\'" . prolog-mode)))

(when (executable-find "swipl")
  (setq prolog-system 'swi))

(use-package ediprolog
  :ensure t
  :commands (ediprolog-dwim)
  :init
  (when (executable-find "swipl")
    (setq ediprolog-system 'swi)))

(with-eval-after-load 'prolog
  (bind-key "<f10>" #'ediprolog-dwim prolog-mode-map))

;;; PYTHON

(with-eval-after-load 'python
  (defun ht/black-format-buffer-file ()
    (interactive)
    (let ((file-name (buffer-file-name))
          (default-directory (project-root (project-current t))))
      (shell-command (format "black -q %s" file-name)))))

(add-hook 'inferior-python-mode-hook #'company-mode)

;;; RUST

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

;;; SML

(use-package sml-mode
  :ensure t
  :commands sml-mode)

;;; TEX

(use-package auctex
  :ensure t
  :hook ((LaTeX-mode . display-line-numbers-mode)))

(with-eval-after-load 'tex
  (setq TeX-auto-save t
        TeX-parse-self t)
  (when (and (is-linux-p) (executable-find "zathura"))
    (add-to-list 'TeX-view-program-list
                 '("zathura" ("zathura" (mode-io-correlate " -P %(outpage)") " %o")))
    (add-to-list 'TeX-view-program-selection
                 '(output-pdf "zathura"))))

(dolist (f '(display-line-numbers-mode
             whitespace-mode))
  (add-hook 'bibtex-mode-hook f))

;;; MISC

(use-package debbugs
  :ensure t
  :commands debbugs-gnu)

(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode)

(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :hook ((markdown-mode . display-line-numbers-mode)
         (markdown-mode . electric-pair-mode)
         (markdown-mode . ht/truncate-lines)))

(use-package rec-mode
  :ensure t
  :if (not (is-windows-p))
  :commands rec-mode)

(use-package yaml-mode
  :ensure t
  :commands yaml-mode
  :hook ((yaml-mode . display-line-numbers-mode)
         (yaml-mode . electric-pair-mode)
         (yaml-mode . ht/truncate-lines)))

;;; GOPHER

(use-package elpher
  :ensure t
  :commands (elpher-browse-url-elpher elpher elpher-go))


;;; --- SHELL --- ;;;

(setq comint-input-ring-size 100000)

(defun ht/shell ()
  (add-to-list 'mode-line-buffer-identification '("" default-directory "  ")))

(dolist (f '(ht/shell
             font-lock-mode))
  (add-hook 'shell-mode-hook f))

(add-hook 'comint-output-filter-functions #'comint-osc-process-output)


;;; --- X11 COPY/PASTE --- ;;;

(when (and (getenv "XTERM_VERSION")
           (executable-find "xsel"))
  (defun xsel-paste ()
    (let ((xsel-output (and (executable-find "xsel" t)
                            (shell-command-to-string "xsel -b"))))
      (if (string-empty-p xsel-output)
          nil
        xsel-output)))
  (setq interprogram-paste-function #'xsel-paste))

;;; --- WAYLAND COPY/PASTE --- ;;;

(when (and (getenv "WAYLAND_DISPLAY")
           (executable-find "wl-copy")
           (executable-find "wl-paste"))
  (defvar wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function #'wl-copy
        interprogram-paste-function #'wl-paste)
  (defun kill-wl-copy-process (arg)
    (when (and wl-copy-process (process-live-p wl-copy-process))
      (kill-process wl-copy-process)))
  (advice-add 'save-buffers-kill-emacs :before #'kill-wl-copy-process)
  nil)


;;; --- POSTLUDE --- ;;;

(defvar ht/clang-format-on-save nil)
(defvar ht/gofmt-on-save nil)
(defvar ht/black-format-on-save nil)

(defun ht/finalize-before-save ()
  (cond ((and (derived-mode-p 'c-mode) ht/clang-format-on-save)
         (clang-format-buffer))
        ((and (derived-mode-p 'go-mode) ht/gofmt-on-save)
         (gofmt))))

(defun ht/finalize-after-save ()
  (cond ((and (derived-mode-p 'python-mode) ht/black-format-on-save)
         (ht/black-format-buffer-file))))

(add-hook 'before-save-hook #'ht/finalize-before-save)
(add-hook 'after-save-hook #'ht/finalize-after-save)

(when (and (is-darwin-p) (display-graphic-p))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

(when (is-windows-p)
  (let ((home (directory-file-name (getenv "USERPROFILE"))))
    (when home
      (setq default-directory home))))

(when (and (is-unix-p) (not (display-graphic-p)))
  (when (xterm-mouse-mode 1)
    (bind-key "<mouse-4>" 'previous-line)
    (bind-key "<mouse-5>" 'next-line)))

;;; REGISTERS

(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))
