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
    (fido-mode 1)))

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

(setq-default fill-column 80
              indent-tabs-mode nil)

(when (is-darwin-p)
  (setq browse-url-browser-function 'browse-url-default-browser
        dired-use-ls-dired nil))

(load custom-file t)

(put 'dired-find-alternate-file 'disabled nil)

(defconst ht/global-bindings
  '(("C-x C-b" . ibuffer)
    ("M-/"     . hippie-expand)
    ("M-z"     . zap-up-to-char)
    ("C-s"     . isearch-forward-regexp)
    ("C-r"     . isearch-backward-regexp)
    ("C-M-s"   . isearch-forward)
    ("C-M-r"   . isearch-backward)))

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

;;; COSMETICS ;;;

(when (version<= "26.1" emacs-version)
  (setq-default display-line-numbers-width 4)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(global-font-lock-mode 0)

(setq frame-background-mode 'light)

(defun ht/set-font-lock-face-attributes ()
  (set-face-attribute 'font-lock-comment-face nil :foreground "#7f7f7f"))

(add-hook 'font-lock-mode-hook #'ht/set-font-lock-face-attributes)

(defun ht/set-face-attributes ()
  (when (and (is-linux-p) (display-graphic-p))
    (set-face-attribute 'region nil :background "lightgoldenrod2"))
  (when (display-graphic-p)
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :box nil)))

(defun ht/fix-split-behavior ()
  (when (and (is-linux-p) (display-graphic-p))
    (setq split-height-threshold nil)))

(defun ht/update-frame (frame)
  (select-frame frame)
  (ht/set-face-attributes)
  (ht/fix-split-behavior))

(add-to-list 'after-make-frame-functions #'ht/update-frame)

(ht/update-frame (selected-frame))

(unless (and (is-linux-p) (window-system))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(when (and (is-windows-p) (window-system))
  (when (member "Dina" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Dina"
                        :foundry 'outline
                        :width 'normal
                        :height 75)))

(show-paren-mode 1)
(column-number-mode 1)

(blink-cursor-mode 0)
(setq visible-cursor nil)
(setq-default cursor-type 'box)

(defun ht/truncate-lines ()
  (setq truncate-lines t))

(dolist (mode-hook '(compilation-mode-hook
                     dired-mode-hook
                     markdown-mode-hook
                     prog-mode-hook
                     shell-mode-hook
                     sql-interactive-mode-hook))
  (add-hook mode-hook #'ht/truncate-lines))

(winner-mode 1)

;;; SITE-LISP ;;;

(add-to-list 'load-path (expand-file-name "paredit" ht/site-lisp-directory))
(autoload 'enable-paredit-mode "paredit.el")

(add-to-list 'load-path (expand-file-name "compile-commands" ht/site-lisp-directory))
(autoload 'compile-commands-get-include-directories "compile-commands.el")

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
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-backends (remove 'company-clang company-backends)
        company-global-modes '(not eshell-mode)))

;;; COMPILE ;;;

(with-eval-after-load 'compile
  (bind-key "<f5>" 'recompile))

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
  :hook ((magit-status-mode . font-lock-mode)
         (magit-diff-mode   . font-lock-mode))
  :config
  (put 'magit-clean 'disabled nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))

;;; MISC EDITING MODES ;;;

(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode)

(use-package markdown-mode
  :ensure t
  :commands markdown-mode)

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

;;; PROG-MODE ;;;

(dolist (mode '(auto-revert-mode
                electric-pair-local-mode))
  (add-hook 'prog-mode-hook mode))

;;; SHELL ;;;

(defun ht/shell ()
  (add-to-list 'mode-line-buffer-identification '("" default-directory "  ")))

(add-hook 'shell-mode-hook #'ht/shell)

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
        whitespace-line-column 80))

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

;;; C/C++ ;;;

(defun ht/modify-c-syntax-entries ()
  (modify-syntax-entry ?_ "w"))

(add-hook 'c-mode-common-hook #'ht/modify-c-syntax-entries)

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
  (add-to-list 'c-default-style '(c-mode   . "bsd-tabs"))
  (add-to-list 'c-default-style '(c++-mode . "hcpp")))

(dolist (mode '(electric-pair-mode))
  (add-hook 'c-mode-hook   mode)
  (add-hook 'c++-mode-hook mode))

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
  :bind ("<f6>" . clang-format-buffer)
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
  :commands cmake-mode
  :mode ("\\.cmake\\'" . cmake-mode))

;;; FORTH ;;;

(use-package forth-mode
  :ensure t
  :commands (forth-mode run-forth))

(use-package forth-block-mode
  :after (forth-mode))

;;; GO ;;;

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  ;; Redefine buggy gofmt defun in go-mode.el
  (defun gofmt ()
    (interactive)
    (let ((pos        (point))
          (in-file    buffer-file-name)
          (out-buffer (get-buffer-create "*gofmt-out*"))
          (err-buffer (get-buffer-create "*gofmt-err*"))
          (temp-file  (make-temp-file "gofmttmp"))
          (err-file   (make-temp-file "gofmterr")))
      (unwind-protect
          (progn
            (write-region (point-min) (point-max) temp-file)
            (let ((result (call-process "gofmt" nil (list out-buffer err-file) nil temp-file)))
              (if (eql result 0)
                  (progn
                    (erase-buffer)
                    (insert-buffer out-buffer)
                    (let ((window (get-buffer-window err-buffer 'visible)))
                      (when window
                        (delete-window window)))
                    (kill-buffer err-buffer)
                    t)
                (with-temp-buffer-window err-buffer 'display-buffer-pop-up-window nil
                  (with-current-buffer err-buffer
                    (erase-buffer)
                    (insert "gofmt found errors:\n")
                    (insert-file-contents err-file)
                    (while (re-search-forward temp-file nil t)
                      (replace-match in-file))
                    (compilation-mode)
                    nil)))))
        (delete-file err-file)
        (delete-file temp-file)
        (kill-buffer out-buffer)
        (goto-char pos))))
  nil)

;;; HASKELL ;;;

(defun ht/haskell-mode ()
  (setq electric-indent-local-mode 0
        haskell-doc-prettify-types nil
        haskell-interactive-popup-errors nil
        haskell-process-log t
        haskell-process-type 'cabal-new-repl
        haskell-stylish-on-save t
        haskell-tags-on-save t))

(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :init
  (dolist (mode '(electric-pair-mode
                  haskell-indentation-mode
                  ht/haskell-mode
                  interactive-haskell-mode))
    (add-hook 'haskell-mode-hook mode)))

;;; SCHEME ;;;

(defun ht/scheme-mode ()
  (dolist (form+n '((conde . 0)
                    (fresh . 1)
                    (run   . 2)
                    (run*  . 1)))
    (put (car form+n) 'scheme-indent-function (cdr form+n))))

(add-hook 'scheme-mode-hook #'ht/scheme-mode)

(add-hook 'scheme-mode-hook 'enable-paredit-mode)

;;; "LISP" (COMMON LISP & ELISP) ;;;

(when (executable-find "sbcl")
  (setq inferior-lisp-program "sbcl"))

(dolist (mode-hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook mode-hook 'enable-paredit-mode))

(use-package sly
  :ensure t
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

(defun ht/setup-tuareg ()
  (when (executable-find "opam")
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var))))
  (let ((ocaml-toplevel-path (getenv "OCAML_TOPLEVEL_PATH")))
    (when ocaml-toplevel-path
      (add-to-list 'load-path (expand-directory-name "../../share/emacs/site-lisp" ocaml-toplevel-path)))))

(use-package tuareg
  :ensure t
  :commands (tuareg-mode tuareg-menhir-mode tuareg-jbuild-mode)
  :init
  (ht/setup-tuareg)
  (dolist (mode '(electric-pair-mode
                  electric-indent-local-mode))
    (add-hook 'tuareg-mode-hook mode)))

(use-package merlin
  :if (and (executable-find "ocamlmerlin")
           (locate-file "merlin.el" load-path))
  :after (tuareg)
  :commands merlin-mode
  :defines merlin-command
  :hook ((caml-mode   . merlin-mode)
         (tuareg-mode . merlin-mode)))
  :init
  (when (executable-find "opam")
    (setq merlin-command 'opam))

(use-package merlin-company
  :if (and (executable-find "ocamlmerlin")
           (locate-file "merlin-company.el" load-path))
  :after (merlin))

(use-package utop
  :if (and (executable-find "utop")
           (locate-file "utop.el" load-path))
  :after (tuareg)
  :commands (utop utop-minor-mode)
  :hook (tuareg-mode . utop-minor-mode))

(use-package ocp-indent
  :if (and (executable-find "ocp-indent")
           (locate-file "ocp-indent.el" load-path))
  :after (tuareg)
  :commands ocp-setup-indent
  :hook (tuareg-mode . ocp-setup-indent))

(with-eval-after-load 'caml-types
  (let ((color (face-attribute 'default :background)))
    (dolist (face '(caml-types-expr-face
                    caml-types-occ-face
                    caml-types-scope-face
                    caml-types-typed-face))
      (set-face-foreground face color))))

(with-eval-after-load 'caml-help
  (set-face-foreground 'ocaml-help-face (face-attribute 'default :background)))

;;; RUST ;;;

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (dolist (mode '(electric-pair-mode))
    (add-hook 'rust-mode-hook mode)))

;;; SML ;;;

(use-package sml-mode
  :ensure t
  :commands sml-mode)

;;; GOPHER ;;;

(use-package elpher
  :ensure t
  :commands (elpher-browse-url-elpher elpher elpher-go))

;;; BUFFER FINALIZATION ;;;

(defvar ht/clang-format-on-save nil)

(defun ht/finalize-buffer ()
  (when (and (derived-mode-p 'c-mode) ht/clang-format-on-save)
    (clang-format-buffer))
  (when (derived-mode-p 'go-mode)
    (gofmt)))

(add-hook 'before-save-hook #'ht/finalize-buffer)

;;; POSTLUDE ;;;

(when (and (is-darwin-p) (window-system))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

(when (is-windows-p)
  (let ((home (directory-file-name (getenv "USERPROFILE"))))
    (when home
      (setq default-directory home))))

;;; REGISTERS ;;;

(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))
