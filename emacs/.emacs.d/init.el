;;; -*- whitespace-line-column: 100; -*-

(message "Loading %s ..." load-file-name)

;;; 1 GB
(setq gc-cons-threshold #x40000000)

(defun ht/reset-gc-cons-threshold ()
  "Resets gc-cons-threshold to its default setting."
  (custom-reevaluate-setting 'gc-cons-threshold))

(defun ht/emacs-ready-msg ()
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'ht/emacs-ready-msg)

;;; https://akrl.sdf.org/#orgc15a10d

(defmacro ht/k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun ht/garbage-collect ()
  (let ((inhibit-message t))
    (message "Garbage Collector has run for %.06fsec"
             (ht/k-time (garbage-collect)))))

(defvar ht/gc-timer (run-with-idle-timer 15 t #'ht/garbage-collect))


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
  (stringp (getenv "IN_NIX_SHELL")))

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
  "Replace or insert PAIR in ALIST based on the key of PAIR.
Return the modified alist."
  (cons pair (assq-delete-all (car pair) alist)))

(defun ht/rassq-replace (alist value pair)
  "Replace or insert PAIR in ALIST based on VALUE.
Return the modified alist."
  (cons pair (rassq-delete-all value alist)))

(defun ht/file-exists-in-project-root-p (file)
  (file-exists-p (expand-file-name file (project-root (project-current t)))))

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
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      compilation-max-output-line-length nil
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      eldoc-echo-area-use-multiline-p nil
      epa-armor t
      gnutls-min-prime-bits 1024
      inhibit-startup-message t
      initial-scratch-message nil
      ispell-program-name "aspell"
      load-prefer-newer t
      mail-envelope-from 'header
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mouse-yank-at-point t
      require-final-newline t
      ring-bell-function 'ignore
      save-interprogram-paste-before-kill t
      save-place-file (concat user-emacs-directory "places")
      scroll-conservatively 1
      send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      tags-revert-without-query t
      uniquify-buffer-name-style 'forward
      visible-bell t
      x-select-enable-primary t
      x-select-enable-clipboard t)

(setq-default case-fold-search nil
              indent-tabs-mode nil)

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
    ("C-r" . isearch-backward-regexp)
    ("C-c h o" . ff-find-other-file)))

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

(defconst ht/site-lisp-directory (expand-file-name "site-lisp" user-emacs-directory))

(ht/comment
  ;; This is a leftover from using Guix's Emacs atop Debian.
  (let ((directory (expand-file-name "/usr/share/emacs/site-lisp")))
    (when (file-directory-p directory)
      (let ((default-directory directory))
        (when (not (member default-directory load-path))
          (push default-directory load-path)
          (normal-top-level-add-subdirs-to-load-path)))))
  ;; Another leftover from using more locally-compiled code and Nix packages
  (mapc #'(lambda (path)
            (when (file-directory-p path)
              (push (expand-file-name path) load-path)))
        '("/usr/local/share/emacs/site-lisp" "~/.nix-profile/share/emacs/site-lisp/"))
  nil)

(require 'package)

(dolist (archive '(("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (add-to-list 'package-archives archive))

(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)
(require 'bind-key)
(setq use-package-verbose t)


;;; --- COSMETICS --- ;;;

;;; https://codeberg.org/dnkl/foot/issues/549#issuecomment-201932
;;; https://codeberg.org/dnkl/foot/wiki#only-8-colors-in-emacs
(add-to-list 'term-file-aliases '("foot" . "xterm"))

(setq-default display-line-numbers-width 4
              display-line-numbers-widen t)

(dolist (mode-hook '(prog-mode-hook
                     conf-mode-hook
                     org-mode-hook
                     text-mode-hook))
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
  (defconst ht/preferred-unix-font "PragmataPro Mono:size=14")
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

(when (or (daemonp) (display-graphic-p))
  (add-to-list 'after-make-frame-functions #'ht/update-frame))

(ht/update-frame (selected-frame))

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

;;; HISTORY

(savehist-mode 1)

;;; EDITING

(defun ht/move-line-up ()
  "Move up the current line or region"
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun ht/move-line-down ()
  "Move down the current line or region"
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(bind-key "M-<up>" #'ht/move-line-up)
(bind-key "M-<down>" #'ht/move-line-down)

;;; TRAMP

(when (version< emacs-version "29.1")
  (add-to-list 'load-path (expand-file-name "docker-tramp" ht/site-lisp-directory))
  (autoload 'docker-tramp-add-method "docker-tramp.el")
  (with-eval-after-load 'tramp
    (docker-tramp-add-method)
    (tramp-set-completion-function docker-tramp-method docker-tramp-completion-function-alist)))

;;; DIRED

(with-eval-after-load 'dired
  (require 'browse-url))

;;; IBUFFER

(setq ibuffer-default-sorting-mode 'filename/process)

;;; XDG

(autoload 'xdg-data-home "xdg.el")

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

;;; COMPANY

(use-package company
  :ensure t
  :commands company-mode
  :hook (((prog-mode shell-mode org-mode) . company-mode))
  :config
  (delete 'company-clang company-backends)
  (delete 'company-xcode company-backends))

;;; LSP

(defun ht/customize-eglot ()
  (eglot-inlay-hints-mode 0))

(use-package eglot
  :ensure t
  :commands eglot
  :init
  (add-hook 'eglot-managed-mode-hook #'ht/customize-eglot)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell (plugin (stan (globalOn . :json-false)))))))

;;; MAGIT

(add-to-list 'load-path (expand-file-name "magit-annex" ht/site-lisp-directory))
(autoload 'magit-annex-dispatch "magit-annex.el")

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
    (when (not (string-equal ht/last-project current-project))
      (setq ht/last-project current-project))))

;;; PROG-MODE

(dolist (f '(auto-revert-mode
             electric-pair-local-mode))
  (add-hook 'prog-mode-hook f))

;;; WHITESPACE

(with-eval-after-load 'whitespace
  (setq whitespace-style '(face trailing)
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

(bind-key "C-c h TAB" #'ht/toggle-tabs-display)

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

(defun ht/set-tab-width (width)
  (interactive "nSet tab-width to: ")
  (setq tab-width width))

;;; AGDA

(when (executable-find "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

(with-eval-after-load 'agda2-mode
  (setq agda2-backend "GHC"))

;;; C/C++

(defun ht/modify-c-syntax-entries ()
  (modify-syntax-entry ?_ "w"))

(add-hook 'c-mode-common-hook #'ht/modify-c-syntax-entries)

(with-eval-after-load 'cc-mode
  (c-add-style "ht" '("k&r" (c-basic-offset . 2) (c-offsets-alist . ((innamespace . [0])))))
  (add-to-list 'c-default-style '(c-mode . "ht"))
  (add-to-list 'c-default-style '(c++-mode . "ht")))

;;; CTAGS

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

(add-to-list 'load-path (expand-file-name "compile-commands" ht/site-lisp-directory))
(autoload 'compile-commands-get-include-directories "compile-commands.el")

(defun ht/project-generate-tags ()
  "Generate TAGS file in the current project's root."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (includes (compile-commands-get-include-directories)))
    (ht/run-ctags includes)))

;;; CLANG-FORMAT

(when (is-windows-p)
  (when-let ((clang-format-path (or (getenv "CLANG_FORMAT_PATH") (executable-find "clang-format"))))
    (let* ((clang-bin-path (file-name-directory clang-format-path))
           (clang-format-load-path (expand-file-name "../share/clang" clang-bin-path)))
      (push clang-format-load-path load-path))))

(use-package clang-format
  :if (locate-file "clang-format.el" load-path)
  :commands (clang-format clang-format-region clang-format-buffer)
  :config
  (when-let ((clang-format-path (getenv "CLANG_FORMAT_PATH")))
    (setq clang-format-executable clang-format-path)))

;;; BISON

(use-package bison-mode
  :ensure t
  :commands bison-mode
  :config
  (setq bison-rule-separator-column 2
        bison-rule-enumeration-column 2
        bison-decl-type-column 0
        bison-decl-token-column 0))

;;; CMAKE

(when (is-windows-p)
  (when-let ((cmake-path (executable-find "cmake")))
    (let* ((cmake-bin-path (file-name-directory cmake-path))
           (cmake-load-path (expand-file-name "../share/emacs/site-lisp" cmake-bin-path)))
      (push cmake-load-path load-path))))

(use-package cmake-mode
  :if (locate-file "cmake-mode.el" load-path)
  :commands cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;;; NINJA

(use-package ninja-mode
  :if (locate-file "ninja-mode.el" load-path)
  :commands ninja-mode)

;;; MESON

(add-to-list 'load-path (expand-file-name "meson-mode" ht/site-lisp-directory))
(autoload 'meson-mode "meson-mode.el")
(add-to-list 'auto-mode-alist '("/meson\\(\\.build\\|_options\\.txt\\)\\'" . meson-mode))

(with-eval-after-load 'meson-mode
  (setq meson-indent-basic 4))

;;; MODULA-2

(with-eval-after-load 'modula2
  (setq m2-indent 3
        m2-compile-command "gm2 -g"))

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
         (haskell-mode . interactive-haskell-mode)
         (haskell-cabal-mode . display-line-numbers-mode)
         (haskell-cabal-mode . whitespace-mode))
  :config
  (setq haskell-doc-prettify-types nil
        haskell-interactive-popup-errors nil
        haskell-process-log t
        haskell-process-type 'cabal-repl
        haskell-stylish-on-save nil
        haskell-tags-on-save nil))

(with-eval-after-load 'haskell-mode
  (defun ht/ormolu-buffer-file ()
    (interactive)
    (let ((file-name (buffer-file-name))
          (default-directory (project-root (project-current t))))
      (shell-command (format "ormolu --mode inplace %s" file-name)))))

;;; ELISP

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

(dolist (f '(ht/add-minikanren-indents))
  (add-hook 'scheme-mode-hook f))

(when-let* ((gsi-path (executable-find "gsi"))
            (gsi (file-name-base gsi-path)))
  (setq scheme-program-name gsi))

(ht/comment
  (when (and (locate-file "gambit.el" load-path)
             (string-equal scheme-program-name "gsi"))
    (autoload 'gambit-mode "gambit.el")
    (autoload 'gambit-inferior-mode "gambit.el")
    (add-hook 'scheme-mode-hook #'gambit-mode)
    (add-hook 'inferior-scheme-mode-hook #'gambit-mode))
  nil)

(use-package geiser-guile
  :ensure t
  :defer t)

(use-package geiser-racket
  :ensure t
  :defer t)

(use-package geiser-mit
  :ensure t
  :defer t)

(setq geiser-mode-auto-p nil)

(add-hook 'geiser-repl-mode-hook #'company-mode)

;;; COMMON LISP

(when (executable-find "sbcl")
  (setq inferior-lisp-program "sbcl"))

(use-package sly
  :ensure t
  :commands sly)

(defun sly-common-lisp-indent-function (indent-point state)
  (common-lisp-indent-function indent-point state))

;;; COQ

(use-package proof-general
  :ensure t
  :defer t)

(use-package coq-mode
  :mode (("\\.v\\'" . coq-mode))
  :commands (coq-mode)
  :hook ((coq-mode . company-coq-mode))
  :config
  (setq proof-colour-locked nil
        proof-splash-enable nil))

;;; company-coq dependencies
(use-package company-math :ensure t)
(use-package yasnippet :ensure t)

(add-to-list 'load-path (expand-file-name "company-coq" ht/site-lisp-directory))
(autoload 'company-coq-mode "company-coq.el")

;;; JAVASCRIPT

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))

(with-eval-after-load 'js
  (setq js-indent-level 2))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;;; LUA

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config
  (setq lua-indent-level 2))

;;; NIX

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(with-eval-after-load 'nix
  (defun ht/nixfmt-buffer-file ()
    (interactive)
    (let ((file-name (buffer-file-name))
          (default-directory (project-root (project-current t))))
      (shell-command (format "nixfmt %s" file-name)))))

;;; OCAML

(defun ht/import-ocaml-env ()
  (when (and (executable-find "opam") (not (in-nix-shell-p)))
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))
    t))

(ht/import-ocaml-env)

(defun ht/get-ocaml-load-path ()
  (when-let* ((ocaml-toplevel-path (getenv "OCAML_TOPLEVEL_PATH")))
    (list (expand-file-name "../../share/emacs/site-lisp" ocaml-toplevel-path))))

(defun ht/is-dune-project-p ()
  (ht/file-exists-in-project-root-p "dune-project"))

(defun ht/set-compile-command-dune ()
  (when (ht/is-dune-project-p)
    (setq-local compile-command "dune build ")))

(use-package dune
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "dune.el" load-path)
  :mode (("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\|\\-workspace\\)?\\'" . dune-mode))
  :commands (dune-mode))

(use-package tuareg
  :ensure t
  :commands (tuareg-mode tuareg-opam-mode)
  :hook ((tuareg-mode . ht/set-compile-command-dune)))

(use-package tuareg-menhir
  :after tuareg
  :commands (tuareg-menhir-mode)
  :mode (("\\.mly\\'" . tuareg-menhir-mode)))

(use-package merlin
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "merlin.el" load-path)
  :commands (merlin-mode)
  :after tuareg
  :hook ((tuareg-mode . merlin-mode)))

(use-package merlin-company
  :after merlin
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "merlin-company.el" load-path))

(use-package ocp-indent
  :disabled t
  :after tuareg
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "ocp-indent.el" load-path)
  :config
  (defun ocp-indent-buffer ()
    (interactive nil)
    (ocp-indent-region 1 (buffer-size))))

(use-package utop
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "utop.el" load-path)
  :commands (utop-minor-mode)
  :after tuareg
  :hook ((tuareg-mode . utop-minor-mode))
  :config
  (when (ht/is-dune-project-p)
    (setq utop-command "dune utop . -- -emacs")))

(with-eval-after-load 'tuareg
  (defun ht/ocamlformat-buffer-file ()
    (interactive)
    (let ((file-name (buffer-file-name))
          (default-directory (project-root (project-current t))))
      (shell-command (format "ocamlformat -i %s" file-name)))))

(use-package opam-switch-mode
  :ensure t
  :if (not (in-nix-shell-p))
  :hook ((coq-mode tuareg-mode) . opam-switch-mode))

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
  :mode "\\.rs\\'"
  :hook ((rust-mode . ht/set-compile-command-cargo))
  :config
  (setq rust-format-on-save t
        rust-format-show-buffer nil
        rust-format-goto-problem nil
        rust-rustfmt-switches '("--edition" "2021")))

(defun ht/is-cargo-project-p ()
  (ht/file-exists-in-project-root-p "Cargo.toml"))

(defun ht/set-compile-command-cargo ()
  (when (ht/is-cargo-project-p)
    (setq-local compile-command "cargo build --all-targets")))

;;; SH

(with-eval-after-load 'sh-script
  (setq sh-basic-offset 2))

;;; SML

(use-package sml-mode
  :ensure t
  :commands sml-mode)

(when (executable-find "poly")
  (setq sml-program-name "poly"))

;;; SWIFT

(use-package swift-mode
  :ensure t
  :commands swift-mode)

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

;;; MARKDOWN

(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :hook ((markdown-mode . display-line-numbers-mode)
         (markdown-mode . electric-pair-mode)
         (markdown-mode . ht/truncate-lines))
  :config
  (require 'url)

  (defun ht/fetch-html-title (url)
    "Fetch the HTML title of a URL."
    (let ((url-buffer (url-retrieve-synchronously url)))
      (save-excursion
        (set-buffer url-buffer)
        (beginning-of-buffer)
        (re-search-forward "<title>\\(.*?\\)</title>" nil t)
        (match-string 1))))

  (defun ht/string-to-ascii (str)
    "Transliterate STR to its closest ASCII representation using iconv."
    (with-temp-buffer
      (insert str)
      (shell-command-on-region (point-min) (point-max) "iconv -f utf-8 -t ascii//translit" nil t)
      (buffer-string)))

  (defun ht/insert-markdown-link-from-url (url)
    "Fetch the HTML title of URL and insert it into the current buffer as a markdown link."
    (interactive "sEnter URL: ")
    (let* ((title (ht/fetch-html-title url))
           (ascii (if title (ht/string-to-ascii title) nil)))
      (cond (ascii (insert (format "[%s](%s)" ascii url)))
            (title (insert (format "[%s](%s)" title url)))
            (t (insert (format "<%s>" url))))))

  (defun ht/convert-url-to-markdown-link ()
    "Take the URL at point, fetch its title and replace it with a markdown link."
    (interactive)
    (let* ((bounds (bounds-of-thing-at-point 'url))
           (url (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (title (ht/fetch-html-title url)))
      (delete-region (car bounds) (cdr bounds))
      (insert (format "[%s](%s)" title url))))

  nil)

;;; MISC

(use-package debbugs
  :ensure t
  :commands debbugs-gnu)

(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode)

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

;;; PAREDIT

(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :hook (((emacs-lisp-mode lisp-data-mode lisp-mode scheme-mode) . enable-paredit-mode)))


;;; --- COMINT --- ;;;

(setq comint-input-ring-size 100000)

(defun ht/shell ()
  (add-to-list 'mode-line-buffer-identification '("" default-directory "  ")))

(dolist (f '(ht/shell
             font-lock-mode))
  (add-hook 'shell-mode-hook f))

(add-hook 'comint-output-filter-functions #'comint-osc-process-output)

;;; Save Comint History
;;;
;;; Refs:
;;; https://web.archive.org/web/20221003174117/https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
;;; https://emacs.stackexchange.com/questions/9720/savehist-the-comint-input-ring

(defun ht/comint-process-sentinel (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))

(defun ht/turn-on-comint-history ()
  (when-let ((derived-mode-p 'comint-mode)
             (process (get-buffer-process (current-buffer)))
             (pname (process-name process))
             (history-file (expand-file-name (format "history-inferior-%s" (downcase pname)) user-emacs-directory)))
    (setq comint-input-ring-file-name history-file)
    (comint-read-input-ring)
    (set-process-sentinel process #'ht/comint-process-sentinel)
    (message "Loading history for %s from %s" pname history-file)
    t))

(add-hook 'tuareg-interactive-mode-hook #'ht/turn-on-comint-history nil nil)


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
  (ht/comment
    ;; This is a bit racy unfortunately
    (defun kill-wl-copy-process (arg)
      (when (and wl-copy-process (process-live-p wl-copy-process))
        (kill-process wl-copy-process)))
    (advice-add 'save-buffers-kill-emacs :before #'kill-wl-copy-process)
    nil)
  nil)


;;; --- POSTLUDE --- ;;;

(setq font-lock-maximum-decoration '((tuareg-mode . 0)
                                     (t . t)))

(defcustom ht/format-on-save nil "Format buffers on save")

(defvar ht/before-save-formatters
  '((c-mode . clang-format-buffer)
    (go-mode . gofmt)))

(defvar ht/after-save-formatters
  '((haskell-mode . ht/ormolu-buffer-file)
    (nix-mode . ht/nixfmt-buffer-file)
    (python-mode . ht/black-format-buffer-file)
    (tuareg-mode . ht/ocamlformat-buffer-file)))

(defun ht/run-formatter (formatters)
  (dolist (mode+formatter formatters)
    (when (derived-mode-p (car mode+formatter))
      (funcall (cdr mode+formatter)))))

(defun ht/finalize-before-save ()
  (when ht/format-on-save
    (ht/run-formatter ht/before-save-formatters)))

(defun ht/finalize-after-save ()
  (when ht/format-on-save
    (ht/run-formatter ht/after-save-formatters)))

(add-hook 'before-save-hook #'ht/finalize-before-save)
(add-hook 'after-save-hook #'ht/finalize-after-save)

(when (and (is-darwin-p) (display-graphic-p))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

(when (is-windows-p)
  (when-let ((home (directory-file-name (getenv "USERPROFILE"))))
    (setq default-directory home)))

(when (and (is-unix-p) (not (display-graphic-p)))
  (when (xterm-mouse-mode 1)
    (bind-key "<mouse-4>" 'previous-line)
    (bind-key "<mouse-5>" 'next-line)))

;;; REGISTERS

(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))
