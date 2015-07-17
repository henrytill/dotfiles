;;;; init.el

(setq browse-url-browser-function 'browse-url-default-browser
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      doc-view-resolution 300
      epa-armor t
      gnutls-min-prime-bits 1024
      ido-handle-duplicate-virtual-buffers 2
      ido-use-virtual-buffers t
      inhibit-startup-message t
      org-directory "~/org"
      ring-bell-function 'ignore
      scroll-conservatively 1)

(setq-default indent-tabs-mode nil      ; also set by better-defaults
              ispell-program-name "aspell")

(load custom-file t)

;;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar common-packages '(better-defaults
                          clojure-mode
                          company
                          dash
                          diminish
                          flycheck
                          haskell-mode
                          idle-highlight-mode
                          inf-clojure
                          magit
                          page-break-lines
                          paredit
                          paren-face
                          pkg-info
                          queue
                          smex
                          sml-mode
                          tuareg
                          undo-tree))

(defvar darwin-packages '(exec-path-from-shell))

(defvar linux-packages '())

;;; helper functions
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

(defun install-my-packages (pkgs)
  (unless (not pkgs)
    (dolist (p pkgs)
      (when (not (package-installed-p p))
        (package-install p)))))

;;; install packages
(install-my-packages common-packages)

(when (is-darwin-p)
  (install-my-packages darwin-packages)
  (exec-path-from-shell-initialize))

(when (is-linux-p)
  (install-my-packages linux-packages))

;; my-site-lisp
(let ((my-site-lisp (expand-directory-name "site-lisp" user-emacs-directory)))
  (when (file-directory-p my-site-lisp)
    (defconst my-site-lisp-path my-site-lisp)))

;;; add site-lisp from ~/.nix-profile
(let ((nix-site-lisp (expand-directory-name "~/.nix-profile/share/emacs/site-lisp/")))
  (when (file-directory-p nix-site-lisp)
    (add-to-list 'load-path nix-site-lisp)))

;;; load files from $HOME/.emacs.d/$USER
(mapc 'load (directory-files (concat user-emacs-directory user-login-name)
                             t "^[^#].*el$"))

;;; Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;; Company
(global-company-mode 1)
(setq company-global-modes '(not eshell-mode))

;;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")
