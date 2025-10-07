;;; init.el --- -*- lexical-binding: t; whitespace-line-column: 100; -*-

;;; Commentary:
;;; Henry's init.el

;;; Code:

(message "Loading %s ..." load-file-name)

;;; 1 GB
(setopt gc-cons-threshold #x40000000)

(defun ht/reset-gc-cons-threshold ()
  "Reset `gc-cons-threshold' to its default setting."
  (custom-reevaluate-setting 'gc-cons-threshold))

(defun ht/emacs-ready-msg ()
  "Print an informative message after startup is complete."
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
  "Run a timed `garbage-collect' and print the duration."
  (let ((inhibit-message t))
    (message "Garbage Collector has run for %.06fsec"
             (ht/k-time (garbage-collect)))))

(defvar ht/gc-timer (run-with-idle-timer 15 t #'ht/garbage-collect))


;;; --- PRELUDE --- ;;;

(defun is-darwin-p ()
  "Return t if the current system is macOS (darwin)."
  (string-equal system-type "darwin"))

(defun is-linux-p ()
  "Return t if the current system is GNU/Linux."
  (string-equal system-type "gnu/linux"))

(defun is-bsd-p ()
  "Return t if the current system is BSD."
  (string-equal system-type "berkeley-unix"))

(defun is-unix-p ()
  "Return t if the current system is any Unix variant (Linux or BSD)."
  (or (is-linux-p) (is-bsd-p)))

(defun is-windows-p ()
  "Return t if the current system is Windows (native or Cygwin)."
  (or (string-equal system-type "windows-nt")
      (string-equal system-type "cygwin")))

(defun in-nix-shell-p ()
  "Return t if running inside a Nix shell."
  (stringp (getenv "IN_NIX_SHELL")))

(defmacro ht/comment (&rest _body)
  "Comment out BODY."
  nil)

(put 'ht/comment 'lisp-indent-function 'defun)

(defun ht/file-exists-in-project-root-p (file)
  "Check if FILE exists in the current project's root directory.

Returns nil if `project-root' is not defined, no project found, or
file doesn't exist."
  (when (fboundp 'project-root)
    (file-exists-p (expand-file-name file (project-root (project-current t))))))

(defun ht/hostname ()
  "Return short hostname if hostname command exists, nil otherwise."
  (when (executable-find "hostname")
    (string-trim (shell-command-to-string "hostname -s"))))

(defun ht/hex-to-decimal (start end)
  "Convert the hexadecimal number in the region from START to END to decimal."
  (interactive "r")
  (let ((input (if (use-region-p)
                   (buffer-substring start end)
                 (string (char-after)))))
    (message (number-to-string (string-to-number input 16)))))

(require 'uniquify)

(setopt apropos-do-all t
        backup-by-copying t
        backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
        compilation-max-output-line-length nil
        custom-file (expand-file-name "custom.el" user-emacs-directory)
        custom-unlispify-remove-prefixes t
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        epa-armor t
        ;; gnus-select-method '(nnnil)
        indent-tabs-mode nil
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
        set-mark-command-repeat-pop t
        tags-revert-without-query t
        uniquify-buffer-name-style 'forward
        visible-bell t
        x-select-enable-primary t
        x-select-enable-clipboard t)

(defgroup ht nil
  "Henry's customization options."
  :prefix "ht/"
  :group 'local)

(load custom-file t)

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(defconst ht/global-bindings
  '(("C-x C-b" . ibuffer)
    ("M-/" . hippie-expand)
    ("M-z" . zap-up-to-char)
    ("C-c h o" . ff-find-other-file)))

(dolist (binding ht/global-bindings)
  (let ((key (car binding))
        (cmd (cdr binding)))
    (global-set-key (kbd key) cmd)))

(setopt which-key-mode t)

;;; https://stackoverflow.com/questions/5147060/how-can-i-access-directory-local-variables-in-my-major-mode-hooks
(defun ht/run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(add-hook 'hack-local-variables-hook #'ht/run-local-vars-mode-hook)


;;; --- LOAD-PATH & PACKAGES --- ;;;

(defconst ht/site-lisp-directory (expand-file-name "site-lisp" user-emacs-directory))

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
(setopt use-package-verbose t)


;;; --- COSMETICS --- ;;;

;;; https://codeberg.org/dnkl/foot/issues/549#issuecomment-201932
;;; https://codeberg.org/dnkl/foot/wiki#only-8-colors-in-emacs
(add-to-list 'term-file-aliases '("foot" . "xterm"))

(setopt display-line-numbers-width 4
        display-line-numbers-widen t)

(dolist (mode-hook '(prog-mode-hook
                     conf-mode-hook
                     org-mode-hook
                     text-mode-hook))
  (add-hook mode-hook #'display-line-numbers-mode))

(setopt frame-background-mode 'dark)

(defun ht/set-face-attributes (frame)
  "Customize fonts for a given FRAME on a graphic display."
  (let ((ht/preferred-unix-font "PragmataPro Mono:size=14")
        (ht/preferred-win-font "PragmataPro Mono:size=12"))
    (when (and (display-graphic-p)
               (fboundp 'set-fontset-font))
      (set-face-attribute 'mode-line frame :box nil)
      (set-face-attribute 'mode-line-inactive frame :box nil)
      (cond ((is-unix-p)
             (progn (set-fontset-font "fontset-default" 'unicode ht/preferred-unix-font)
                    (set-face-attribute 'default frame :font ht/preferred-unix-font)
                    (set-face-attribute 'fixed-pitch frame :font ht/preferred-unix-font)))
            ((is-windows-p)
             (progn (set-fontset-font "fontset-default" 'unicode ht/preferred-win-font)
                    (set-face-attribute 'default frame :font ht/preferred-win-font)))))))

(defun ht/remove-decorations ()
  "Remove decorations."
  (when (and (is-unix-p)
             (not (display-graphic-p)))
    (menu-bar-mode -1))
  (when (display-graphic-p)
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
    (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))))

(defun ht/fix-split-behavior ()
  "Fix split behavior on Unix with a graphic display."
  (when (and (is-unix-p) (display-graphic-p))
    (setopt split-height-threshold nil)))

(defun ht/update-frame (frame)
  "Customize a given FRAME."
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
(setopt visible-cursor nil)

(defun ht/truncate-lines ()
  "Toggle truncating of long lines for the current buffer."
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

(setopt display-time-day-and-date t
        display-time-default-load-average nil)

(display-time-mode 1)


;;; --- GENERAL --- ;;;

;;; NAVIGATION

(defun ht/next-page ()
  "Move to the next page and narrow to it."
  (interactive)
  (narrow-to-page 1))

(defun ht/prev-page ()
  "Move to the previous page and narrow to it."
  (interactive)
  (narrow-to-page -1)
  (goto-char (point-min)))

(bind-key "C-c n n" #'ht/next-page)
(bind-key "C-c n p" #'ht/prev-page)

;;; HISTORY

(savehist-mode 1)

;;; EDITING

(defun ht/move-line-up ()
  "Move up the current line or region."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun ht/move-line-down ()
  "Move down the current line or region."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(bind-key "M-<up>" #'ht/move-line-up)
(bind-key "M-<down>" #'ht/move-line-down)

;;; CONF-MODE

(defun ht/customize-conf-unix ()
  "Customize `conf-unix-mode'."
  (when indent-tabs-mode
    (setq-local indent-line-function 'insert-tab)))

(add-hook 'conf-unix-mode-local-vars-hook #'ht/customize-conf-unix)

;;; DIRED

(with-eval-after-load 'dired
  (require 'browse-url))

(setopt dired-dwim-target t
        dired-listing-switches "-al --group-directories-first")

;;; IBUFFER

(setopt ibuffer-default-sorting-mode 'filename/process)

;;; XDG

(autoload 'xdg-data-home "xdg.el")

;;; INFO

(when (and (is-linux-p) (fboundp 'xdg-data-home))
  (require 'info)
  (when (boundp 'Info-additional-directory-list)
    (add-to-list 'Info-additional-directory-list (expand-file-name "info" (xdg-data-home)))))

;;; ORG-MODE

(with-eval-after-load 'org
  (message "Loading org config...")
  (require 'oc-csl)
  (when (boundp 'org-file-apps)
    (add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "zathura -P %1 %s")))
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (haskell . t)
                                                           (shell . t)))
  nil)

;;; LSP

(defun ht/customize-eglot ()
  "Customize `eglot-mode'."
  (eglot-inlay-hints-mode 0))

(use-package eglot
  :ensure t
  :commands eglot
  :hook ((eglot-managed-mode . ht/customize-eglot))
  :functions (eglot-inlay-hints-mode)
  :config
  (setq eglot-workspace-configuration
        '((haskell (plugin (stan (globalOn . :json-false)))))))

;;; MAGIT

(use-package magit
  :ensure t
  :commands (magit-status magit-project-status)
  :hook ((magit-status-mode . font-lock-mode)
         (magit-diff-mode . font-lock-mode))
  :defines (magit-mode-map)
  :config
  (put 'magit-clean 'disabled nil)
  (setopt magit-last-seen-setup-instructions "1.4.0")
  (bind-key "@" #'magit-annex-dispatch magit-mode-map))

(use-package magit-annex
  :after magit
  :vc (:url "https://github.com/magit/magit-annex.git" :rev :newest)
  :commands magit-annex-dispatch)

;;; EAT

(use-package eat
  :ensure t
  :commands (eat eat-project))

;;; PROJECT

(setopt project-vc-extra-root-markers '(".dir-locals.el"))

(defun ht/is-make-project-p ()
  "Return t if there is a Makefile or GNUmakefile at the project root."
  (or (ht/file-exists-in-project-root-p "Makefile")
      (ht/file-exists-in-project-root-p "GNUmakefile")))

;;; PROG-MODE

(dolist (f '(auto-revert-mode
             electric-pair-local-mode))
  (add-hook 'prog-mode-hook f))

;;; TREE-SITTER

(with-eval-after-load 'treesit
  (when (boundp 'treesit-language-source-alist)
    (setq treesit-language-source-alist
          '((haskell "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
            (json "https://github.com/tree-sitter/tree-sitter-json")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
            (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3" "src")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))))

;;; WHITESPACE

(with-eval-after-load 'whitespace
  (setopt whitespace-style '(face trailing)
          whitespace-line-column 100))

(add-hook 'prog-mode-hook #'whitespace-mode)

(defun ht/toggle-tabs-display ()
  "Toggle the display of tab characters in `whitespace-mode'."
  (interactive)
  (whitespace-mode -1)
  (let ((tab-mode (if font-lock-mode 'tabs 'tab-mark)))
    (when (boundp 'whitespace-style)
      (if (memq tab-mode whitespace-style)
          (setopt whitespace-style (remove tab-mode whitespace-style))
        (add-to-list 'whitespace-style tab-mode))))
  (whitespace-mode 1))

(bind-key "C-c h TAB" #'ht/toggle-tabs-display)

(defun ht/hide-lines-tail-display ()
  "Disable the display of lines exceeding the maximum length in `whitespace-mode'."
  (interactive)
  (whitespace-mode -1)
  (when (and (boundp 'whitespace-style)
             (memq 'lines-tail whitespace-style))
    (setopt whitespace-style (remove 'lines-tail whitespace-style)))
  (whitespace-mode 1))

(defun ht/toggle-lines-tail-display ()
  "Toggle the display of lines exceeding the maximum length in `whitespace-mode'."
  (interactive)
  (whitespace-mode -1)
  (when (boundp 'whitespace-style)
    (if (memq 'lines-tail whitespace-style)
        (setopt whitespace-style (remove 'lines-tail whitespace-style))
      (add-to-list 'whitespace-style 'lines-tail)))
  (whitespace-mode 1))

;;; TEXT

(add-hook 'text-mode-hook 'auto-revert-mode)


;;; --- PROGRAMMING LANGUAGES --- ;;;

;;; AGDA

(when (executable-find "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  (setopt agda2-mode-abbrevs-use-defaults t))

(with-eval-after-load 'agda2-mode
  (setopt agda2-backend "GHC"))

;;; C/C++

(defun ht/modify-c-syntax-entries ()
  "Modify syntax table entries for C/C++ mode to treat underscore as part of words."
  (modify-syntax-entry ?_ "w"))

(defun ht/c-mode-common ()
  "Common setup for C/C++ modes."
  (ht/modify-c-syntax-entries)
  (remove-hook 'flymake-diagnostic-functions #'flymake-cc t))

(add-hook 'c-mode-common-hook #'ht/c-mode-common)

(with-eval-after-load 'cc-mode
  (c-add-style "ht" '("k&r" (c-basic-offset . 2) (c-offsets-alist . ((innamespace . [0])))))
  (when (boundp 'c-default-style)
    (add-to-list 'c-default-style '(c-mode . "ht"))
    (add-to-list 'c-default-style '(c++-mode . "ht"))
    (add-to-list 'c-default-style '(java-mode . "ht"))
    (add-to-list 'c-default-style '(awk-mode . "ht"))))

(put 'ff-search-directories 'safe-local-variable #'listp)

;;; CTAGS

(defvar path-to-ctags "ctags" "Variable to store the path to the ctags executable.")

(defun ht/run-ctags (paths)
  "Run ctags on the specified PATHS with default arguments for C and C++."
  (let* ((args '("-e" "--langmap=c:.c.h" "--c-kinds=+fp" "-R"))
         (arg-string (mapconcat 'identity args " "))
         (paths-string (mapconcat 'identity paths " "))
         (cmd-string (format "%s %s %s" path-to-ctags arg-string paths-string)))
    (message "Running: %s" cmd-string)
    (shell-command cmd-string)))

(defun ht/generate-tags (dir-name)
  "Generate TAGS file for the directory DIR-NAME."
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
    (setopt clang-format-executable clang-format-path)))

;;; BISON

(use-package bison-mode
  :ensure t
  :commands bison-mode
  :config
  (setopt bison-rule-separator-column 2
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

(use-package meson-mode
  :vc (:url "https://github.com/wentasah/meson-mode.git" :rev :newest)
  :mode (("/meson\\(\\.build\\|_options\\.txt\\)\\'" . meson-mode))
  :commands meson-mode
  :config
  (setopt meson-indent-basic 4)
  (defun ht/meson-format-buffer-file ()
    "Format the current Meson buffer."
    (interactive)
    (let ((file-name (buffer-file-name))
          (default-directory (project-root (project-current t))))
      (shell-command (format "meson format -i %s" file-name)))))

;;; MODULA-2

(with-eval-after-load 'modula2
  (setopt m2-indent 3
          m2-compile-command "gm2 -g"))

;;; APL

(use-package gnu-apl-mode
  :ensure t
  :commands (gnu-apl gnu-apl-mode)
  :init
  (setopt gnu-apl-show-keymap-on-startup nil))

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

(defun setup-go-eglot ()
  "Configure eglot to use gopls from GOPATH if not available in PATH."
  (when (and (executable-find "go")
             (not (executable-find "gopls")))
    (let* ((gopath-output (shell-command-to-string "go env GOPATH"))
           (gopath (string-trim gopath-output))
           (gopls-path (expand-file-name "bin/gopls" gopath)))
      (when (file-exists-p gopls-path)
        (with-eval-after-load 'eglot
          (cl-flet ((go-entry-p (entry)
                      (let ((modes (car entry)))
                        (and (listp modes)
                             (memq 'go-mode modes)))))
            (let ((go-entry (cl-find-if #'go-entry-p eglot-server-programs)))
              (when go-entry
                (setq eglot-server-programs
                      (cons `(,(car go-entry) . (,gopls-path))
                            (remove go-entry eglot-server-programs)))))))))))

(setup-go-eglot)

;;; HASKELL

(defun ht/fourmolu-buffer-file ()
  "Format the current Haskell buffer using the fourmolu formatter."
  (interactive)
  (let ((file-name (buffer-file-name))
        (default-directory (project-root (project-current t))))
    (shell-command (format "fourmolu -q --mode inplace %s" file-name))))

(defun ht/run-ghc-tags ()
  "Run ghc-tags on the current Haskell project to generate a TAGS file."
  (interactive)
  (when (derived-mode-p 'haskell-mode)
    (if (executable-find "ghc-tags")
        (let ((default-directory (project-root (project-current t)))
              (inhibit-message t))
          (shell-command "ghc-tags -e"))
      (message "ghc-tags not found"))))

(defun ht/customize-haskell ()
  "Customize Haskell mode with appropriate settings and hooks."
  (setq-local compile-command "cabal v2-build all")
  ;; We shouldn't need to do this
  (when (fboundp 'haskell-indentation-mode)
    (haskell-indentation-mode 0))
  (add-hook 'after-save-hook #'ht/run-ghc-tags nil t))

(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . ht/customize-haskell)
         (haskell-cabal-mode . display-line-numbers-mode)
         (haskell-cabal-mode . whitespace-mode))
  :config
  (remove-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (setopt haskell-doc-prettify-types nil
          haskell-interactive-popup-errors nil
          haskell-process-log t
          haskell-process-show-debug-tips nil
          haskell-process-type 'cabal-repl
          haskell-stylish-on-save nil
          haskell-tags-on-save nil)
  nil)

;;; IDRIS

(use-package idris-mode
  :vc (:url "https://github.com/idris-hackers/idris-mode.git" :rev :newest)
  :commands (idris-mode idris-ipkg-mode)
  :mode (("\\.idr\\'" . idris-mode)
         ("\\.ipkg\\'" . idris-ipkg-mode))
  :config
  (setopt idris-interpreter-path "idris2"))

;;; ELISP

(use-package macrostep
  :ensure t
  :commands macrostep-expand)

(bind-key "C-c e" #'macrostep-expand emacs-lisp-mode-map)

;;; SCHEME

(defvar ht/minikanren-indents
  '((conde . 0)
    (fresh . 1)
    (run . 2)
    (run* . 1)))

(defvar ht/chez-indents
  '((begin . 0)
    (call-with-current-continuation . 0)
    (call/cc . 0)
    (cond . 0)
    (library . 1)
    (make-compile-time-value . 0)))

(defvar ht/custom-indents
  '((comment . 0)
    (test . 1)
    (test-error . 1)))

(defun ht/add-scheme-indents (indent-list)
  "Set indentation from given INDENT-LIST."
  (dolist (form+n indent-list)
    (put (car form+n) 'scheme-indent-function (cdr form+n))))

(defun ht/add-custom-scheme-indents ()
  "Set indentation for custom scheme forms."
  (dolist (indents (list ht/minikanren-indents
                         ht/chez-indents
                         ht/custom-indents))
    (ht/add-scheme-indents indents)))

(add-hook 'scheme-mode-hook #'ht/add-custom-scheme-indents)

(when-let* ((chez-path (executable-find "chezscheme"))
            (chez (file-name-base chez-path)))
  (setopt scheme-program-name chez))

(setopt scheme-mit-dialect nil)

(use-package geiser-chez
  :ensure t
  :defer t
  :config
  (setopt geiser-mode-auto-p nil
          geiser-chez-binary "chezscheme"
          geiser-chez-csug-url "file:///usr/share/doc/chezscheme-doc/csug9.5/"))

;;; RACKET

(defun ht/add-racket-indents ()
  "Set indentation for Racket-specific forms."
  (dolist (form+n '((test-suite . 1)))
    (put (car form+n) 'racket-indent-function (cdr form+n))))

(use-package racket-mode
  :ensure t
  :defer t
  :hook ((racket-mode . racket-xp-mode)
         (racket-mode . ht/add-racket-indents)))

;;; COMMON LISP

(when (executable-find "sbcl")
  (setopt inferior-lisp-program "sbcl"))

(use-package sly
  :ensure t
  :commands sly)

(defun sly-common-lisp-indent-function (indent-point state)
  "Function to indent the arguments of a Lisp function call.

This is suitable for use as the value of the variable
`lisp-indent-function'.  INDENT-POINT is the point at which the
indentation function is called, and STATE is the `parse-partial-sexp'
state at that position."
  (common-lisp-indent-function indent-point state))

;;; COQ

(use-package proof-general
  :ensure t
  :defer t)

(use-package coq-mode
  :mode (("\\.v\\'" . coq-mode))
  :commands (coq-mode)
  :config
  (setopt proof-colour-locked nil
          proof-splash-enable nil
          proof-three-window-mode-policy 'hybrid))

(use-package rocq-mode
  :disabled t
  :vc (:url "https://codeberg.org/jpoiret/rocq-mode.el.git" :rev :newest)
  :mode "\\.v\\'"
  :hook
  (rocq-mode . rocq-follow-viewport-mode)
  (rocq-mode . rocq-auto-goals-at-point-mode))

(use-package yasnippet :ensure t :defer t)

;;; JAVASCRIPT

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))

(with-eval-after-load 'js
  (when (boundp 'js-mode-map)
    (bind-key "M-." nil js-mode-map))
  (setopt js-indent-level 2)
  (defun ht/prettier-buffer-file ()
    "Format the current JavaScript buffer using prettier."
    (interactive)
    (let ((file-name (buffer-file-name))
          (default-directory (project-root (project-current t))))
      (shell-command (format "npx prettier --write %s" file-name)))))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;;; JULIA

(use-package julia-mode
  :ensure t
  :defer t)

;;; LUA

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config
  (setopt lua-indent-level 2))

;;; NIX

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(with-eval-after-load 'nix
  (defun ht/nixfmt-buffer-file ()
    "Format the current Nix buffer using nixfmt."
    (interactive)
    (let ((file-name (buffer-file-name))
          (default-directory (project-root (project-current t))))
      (shell-command (format "nixfmt %s" file-name)))))

;;; OCAML

(defun ht/import-ocaml-env ()
  "Import opam environment variables for OCaml development."
  (when (and (executable-find "opam") (not (in-nix-shell-p)))
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))
    t))

(ht/import-ocaml-env)

(defun ht/get-ocaml-load-path ()
  "Get the load path for OCaml-related Emacs packages."
  (when-let ((ocaml-toplevel-path (getenv "OCAML_TOPLEVEL_PATH")))
    (list (expand-file-name "../../share/emacs/site-lisp" ocaml-toplevel-path))))

(defun ht/is-dune-project-p ()
  "Return t if the current project is a Dune project."
  (ht/file-exists-in-project-root-p "dune-project"))

(defun ht/set-compile-command-dune ()
  "Set the compile command to `dune build @all' if in a Dune project."
  (when (and (not (ht/is-make-project-p))
             (ht/is-dune-project-p))
    (setq-local compile-command "dune build @all")))

(use-package dune
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "dune.el" load-path)
  :mode (("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\|\\-workspace\\)?\\'" . dune-mode))
  :commands (dune-mode))

(use-package tuareg
  :ensure t
  :commands (tuareg-mode tuareg-opam-mode)
  :hook ((tuareg-mode . ht/set-compile-command-dune))
  :config
  (when (not (fboundp 'ht/ocamlformat-buffer-file))
    (defun ht/ocamlformat-buffer-file ()
      "Format the current OCaml buffer using ocamlformat."
      (interactive)
      (let ((file-name (buffer-file-name))
            (default-directory (project-root (project-current t))))
        (shell-command (format "ocamlformat -i %s" file-name))))))

(use-package merlin
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "merlin.el" load-path)
  :commands (merlin-mode)
  :hook ((tuareg-mode . merlin-mode)))

(use-package ocp-indent
  :disabled t
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
  :hook ((tuareg-mode . utop-minor-mode))
  :config
  (when (ht/is-dune-project-p)
    (setopt utop-command "dune utop . -- -emacs")))

(use-package opam-switch-mode
  :ensure t
  :if (not (in-nix-shell-p))
  :hook ((coq-mode tuareg-mode) . opam-switch-mode))

;;; PERL

(use-package perl-doc
  :ensure t
  :defer t)

(defun ht/customize-perl ()
  "Customize `perl-mode'."
  (flymake-mode 1)
  (setopt perl-indent-continued-arguments nil)
  (when indent-tabs-mode
    (setq-local perl-indent-level 8
                perl-continued-statement-offset 8
                perl-continued-brace-offset 0
                perl-brace-offset -8
                perl-brace-imaginary-offset 0
                perl-label-offset -8)))

(add-hook 'perl-mode-local-vars-hook #'ht/customize-perl)

;;; PROLOG

(use-package ediprolog
  :ensure t
  :commands (ediprolog-dwim))

;;; PYTHON

(with-eval-after-load 'python
  (defun ht/black-format-buffer-file ()
    "Format the current Python buffer using black formatter."
    (interactive)
    (let ((file-name (buffer-file-name))
          (default-directory (project-root (project-current t))))
      (shell-command (format "black -q %s" file-name)))))

;;; RUST

(defun ht/rust-mode ()
  "Set up skeleton pairs for Rust mode."
  (setq-local skeleton-pair t
              skeleton-pair-alist '((?| _ ?|) (?\|))))

(defun ht/is-cargo-project-p ()
  "Return t if the current project is a Cargo project."
  (ht/file-exists-in-project-root-p "Cargo.toml"))

(defun ht/set-compile-command-cargo ()
  "Set the compile command to `cargo build --all-targets' if in a Cargo project."
  (when (and (not (ht/is-make-project-p))
             (ht/is-cargo-project-p))
    (setq-local compile-command "cargo build --all-targets")))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook ((rust-mode . ht/set-compile-command-cargo)
         (rust-mode . ht/rust-mode))
  :bind (("<" . skeleton-pair-insert-maybe)
         ("|" . skeleton-pair-insert-maybe))
  :init
  (setopt rust-mode-treesitter-derive t)
  :config
  (setopt rust-format-on-save t
          rust-format-show-buffer nil
          rust-format-goto-problem nil
          rust-rustfmt-switches '("--edition" "2024")))

;;; SML

(use-package sml-mode
  :ensure t
  :commands sml-mode)

(when (executable-find "poly")
  (setopt sml-program-name "poly"))

;;; SWIFT

(use-package swift-mode
  :ensure t
  :commands swift-mode)

;;; TEX

(defun ht/customize-auctex ()
  "Customize `LaTeX-mode'."
  (when (and (fboundp 'TeX-source-correlate-mode)
             (fboundp 'TeX-PDF-mode))
    (TeX-source-correlate-mode 1)
    (TeX-PDF-mode 1)))

(use-package auctex
  :ensure t
  :hook ((LaTeX-mode . display-line-numbers-mode)
         (LaTeX-mode . ht/customize-auctex)))

(with-eval-after-load 'tex
  (setopt TeX-auto-save t
          TeX-parse-self t)
  (when (and (is-linux-p)
             (executable-find "zathura")
             (boundp 'TeX-view-program-selection))
    (add-to-list 'TeX-view-program-selection
                 '(output-pdf "Zathura"))))

(dolist (f '(display-line-numbers-mode
             whitespace-mode))
  (add-hook 'bibtex-mode-hook f))

;;; ZIG

(use-package zig-mode
  :vc (:url "https://github.com/henrytill/zig-mode.git" :rev "patched")
  :commands zig-mode
  :mode (("\\.zig\\'" . zig-mode))
  :config
  (defun ht/zig-fmt-buffer-file ()
    "Format the current Zig buffer using zig fmt."
    (interactive)
    (let ((file-name (buffer-file-name))
          (default-directory (project-root (project-current t))))
      (shell-command (format "zig fmt %s" file-name)))))

;;; CSV

(use-package csv-mode
  :ensure t
  :defer t)

;;; MARKDOWN

(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :hook ((markdown-mode . display-line-numbers-mode)
         (markdown-mode . electric-pair-mode)
         (markdown-mode . ht/truncate-lines))
  :functions (ht/fetch-html-title
              ht/string-to-ascii
              ht/insert-markdown-link-from-url)
  :config
  (require 'url)

  (defun ht/fetch-html-title (url)
    "Fetch the HTML title of a URL."
    (let ((url-buffer (url-retrieve-synchronously url)))
      (with-current-buffer url-buffer
        (goto-char (point-min))
        (re-search-forward "<title>\\(.*?\\)</title>" nil t)
        (match-string 1))))

  (defun ht/string-to-ascii (str)
    "Transliterate STR to its closest ASCII representation using iconv."
    (with-temp-buffer
      (insert str)
      (shell-command-on-region (point-min) (point-max) "iconv -f utf-8 -t ascii//translit" nil t)
      (buffer-string)))

  (defun ht/insert-markdown-link-from-url (url)
    "Fetch the HTML title of URL and insert it into the current buffer
as a markdown link."
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

;;; SH-MODE

(add-to-list 'auto-mode-alist '("\\.bash_functions\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'" . sh-mode))

(setopt sh-basic-offset 4
        sh-indent-for-case-label 0
        sh-indent-for-case-alt '+)

(defun ht/customize-rc ()
  "Customize `sh-mode' for rc shell."
  (setq indent-line-function 'insert-tab)
  (when (boundp 'sh-mode-syntax-table)
    (modify-syntax-entry ?` "." sh-mode-syntax-table)))

(defun ht/customize-sh ()
  "Customize `sh-mode'."
  (indent-tabs-mode t)
  (electric-indent-local-mode 1)
  (when (and (boundp 'sh-shell)
             (eq sh-shell 'rc))
    (ht/customize-rc))
  (when (and (boundp 'sh-shell)
             (eq sh-shell 'bash)
             (executable-find "shellcheck"))
    (flymake-mode 1)))

(add-hook 'sh-mode-hook #'ht/customize-sh)

;;; GOPHER

(use-package elpher
  :ensure t
  :commands (elpher-browse-url-elpher elpher elpher-go))

;;; PAREDIT

(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :hook (((emacs-lisp-mode lisp-data-mode lisp-mode racket-mode scheme-mode) . enable-paredit-mode)))

(add-hook 'lisp-interaction-mode-hook 'disable-paredit-mode)


;;; --- COMINT --- ;;;

(setopt comint-input-ring-size 100000)

(defun ht/shell ()
  "Customize `shell-mode'."
  (add-to-list 'mode-line-buffer-identification '("" default-directory "  ")))

(dolist (f '(ht/shell
             font-lock-mode))
  (add-hook 'shell-mode-hook f))

(when (fboundp 'comint-osc-process-output)
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output))

;;; Save Comint History
;;;
;;; Refs:
;;; https://web.archive.org/web/20221003174117/https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
;;; https://emacs.stackexchange.com/questions/9720/savehist-the-comint-input-ring

(defcustom ht/use-project-comint-history nil
  "Use project-specific comint history files."
  :type 'boolean
  :group 'ht)

(defun ht/comint-process-sentinel (process event)
  "Save the comint input ring on EVENT from PROCESS."
  (when (fboundp 'comint-write-input-ring)
    (comint-write-input-ring)
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (insert (format "\nProcess %s %s" process event)))))))

(defun ht/get-history-file (pname)
  "Get the history file path for process with name PNAME."
  (let ((filename (format "history-inferior-%s" (downcase pname))))
    (hack-local-variables)
    (if ht/use-project-comint-history
        (expand-file-name (concat "." filename) (project-root (project-current t)))
      (expand-file-name filename user-emacs-directory))))

(defun ht/turn-on-comint-history ()
  "Enable persistent history for comint-based modes."
  (when (fboundp 'comint-read-input-ring)
    (when-let* ((_ (derived-mode-p 'comint-mode))
                (process (get-buffer-process (current-buffer)))
                (pname (process-name process))
                (history-file (ht/get-history-file pname)))
      (setq-local comint-input-ring-file-name history-file)
      (comint-read-input-ring)
      (set-process-sentinel process #'ht/comint-process-sentinel)
      (message "Loading history for %s from %s" pname history-file)
      t)))

(add-hook 'inferior-scheme-mode-hook #'ht/turn-on-comint-history nil nil)
(add-hook 'tuareg-interactive-mode-hook #'ht/turn-on-comint-history nil nil)


;;; --- COPY/PASTE --- ;;;

;;; X11

(when (and (getenv "XTERM_VERSION")
           (executable-find "xsel"))
  (defun xsel-cut (text &optional _push)
    (when (and text (executable-find "xsel" t))
      (let ((proc (make-process :name "xsel"
                                :buffer nil
                                :command '("xsel" "-b" "-i")
                                :connection-type 'pipe)))
        (process-send-string proc text)
        (process-send-eof proc))))
  (defun xsel-paste ()
    (let ((xsel-output (and (executable-find "xsel" t)
                            (shell-command-to-string "xsel -b"))))
      (if (string-empty-p xsel-output)
          nil
        xsel-output)))
  (when (and (fboundp 'xsel-cut)
             (fboundp 'xsel-paste))
    (setq interprogram-cut-function #'xsel-cut
          interprogram-paste-function #'xsel-paste)))

;;; WAYLAND

(when (and (not (display-graphic-p))
           (getenv "WAYLAND_DISPLAY")
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
  (when (and (fboundp 'wl-copy)
             (fboundp 'wl-paste))
    (setq interprogram-cut-function #'wl-copy
          interprogram-paste-function #'wl-paste))
  nil)


;;; --- POSTLUDE --- ;;;

(setopt font-lock-maximum-decoration '((tuareg-mode . 0)
                                       (t . t)))

(defcustom ht/format-on-save nil
  "Format buffers on save."
  :type 'boolean
  :group 'ht)

(defvar ht/before-save-formatters
  '((c-mode . clang-format-buffer)
    (c++-mode . clang-format-buffer)
    (java-mode . clang-format-buffer)
    (go-mode . gofmt)))

(defvar ht/after-save-formatters
  '((haskell-mode . ht/fourmolu-buffer-file)
    (js-mode . ht/prettier-buffer-file)
    (meson-mode . ht/meson-format-buffer-file)
    (nix-mode . ht/nixfmt-buffer-file)
    (python-mode . ht/black-format-buffer-file)
    (tuareg-mode . ht/ocamlformat-buffer-file)
    (zig-mode . ht/zig-fmt-buffer-file)))

(defun ht/run-formatter (formatters)
  "Run appropriate formatter from FORMATTERS based on current major mode."
  (dolist (mode+formatter formatters)
    (when (derived-mode-p (car mode+formatter))
      (funcall (cdr mode+formatter)))))

(defun ht/finalize-before-save ()
  "Run formatters before saving if `ht/format-on-save' is enabled."
  (when ht/format-on-save
    (ht/run-formatter ht/before-save-formatters)))

(defun ht/finalize-after-save ()
  "Run formatters after saving if `ht/format-on-save' is enabled."
  (when ht/format-on-save
    (ht/run-formatter ht/after-save-formatters)))

(add-hook 'before-save-hook #'ht/finalize-before-save)
(add-hook 'after-save-hook #'ht/finalize-after-save)

(when (and (is-darwin-p) (display-graphic-p))
  (setopt mac-command-modifier 'super
          mac-option-modifier 'meta))

(when (is-windows-p)
  (when-let ((home (directory-file-name (getenv "USERPROFILE"))))
    (setq default-directory home)))

(when (and (is-unix-p) (not (display-graphic-p)))
  (xterm-mouse-mode 1)
  (mouse-wheel-mode 1))

(when (string-equal "proteus" (ht/hostname))
  (display-battery-mode 1))

;;; REGISTERS

(set-register ?i `(file . ,(locate-user-emacs-file "init.el")))

(provide 'init)
;;; init.el ends here
