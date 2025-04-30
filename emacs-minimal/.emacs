;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(modus-operandi))
 '(dired-dwim-target 'dired-dwim-target-next)
 '(dired-listing-switches "-al --group-directories-first")
 '(eglot-ignored-server-capabilities '(:inlayHintProvider))
 '(fido-mode t)
 '(fido-vertical-mode t)
 '(frame-background-mode 'light)
 '(global-display-line-numbers-mode t)
 '(haskell-mode-hook '(interactive-haskell-mode ht/customize-haskell-mode))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(cmake-mode company haskell-mode magit nix-mode rust-mode tuareg))
 '(prog-mode-hook '(electric-pair-local-mode))
 '(require-final-newline t)
 '(savehist-mode t)
 '(truncate-lines t)
 '(visible-cursor nil)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun ht/file-exists-in-project-root-p (file)
  "Check if FILE exists in the current project's root directory.

Returns nil if `project-root' is not defined, no project found, or
file doesn't exist."
  (when (fboundp 'project-root)
    (file-exists-p (expand-file-name file (project-root (project-current t))))))

(defun ht/customize-haskell-mode ()
  "Customize Haskell mode with appropriate settings."
  ;; We shouldn't need to do this
  (when (fboundp 'haskell-indentation-mode)
    (haskell-indentation-mode 0)))

(defun ht/import-ocaml-env ()
  "Import opam environment variables for OCaml development."
  (when (and (not (getenv "OCAML_TOPLEVEL_PATH"))
	     (executable-find "opam"))
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))))

(ht/import-ocaml-env)

(defun ht/get-ocaml-load-path ()
  "Get the load path for OCaml-related Emacs packages."
  (when-let ((ocaml-toplevel-path (getenv "OCAML_TOPLEVEL_PATH")))
    (list (expand-file-name "../../share/emacs/site-lisp" ocaml-toplevel-path))))

(defun ht/is-dune-project-p ()
  "Return t if the current project is a Dune project."
  (ht/file-exists-in-project-root-p "dune-project"))

(use-package dune
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "dune.el" load-path)
  :mode (("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\|\\-workspace\\)?\\'" . dune-mode))
  :commands (dune-mode))

(use-package merlin
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "merlin.el" load-path)
  :commands (merlin-mode)
  :hook ((tuareg-mode . merlin-mode)))

(use-package merlin-company
  :after merlin
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "merlin-company.el" load-path))

(use-package utop
  :load-path (lambda () (ht/get-ocaml-load-path))
  :if (locate-file "utop.el" load-path)
  :commands (utop-minor-mode utop)
  :hook ((tuareg-mode . utop-minor-mode))
  :config
  (when (ht/is-dune-project-p)
    (setopt utop-command "dune utop . -- -emacs")))
