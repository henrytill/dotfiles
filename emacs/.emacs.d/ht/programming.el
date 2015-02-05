;;;; Programming Modes

(add-hook 'prog-mode-hook 'undo-tree-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)

;;; Flycheck
(require 'flycheck)

(flycheck-define-checker racket-alt
  "A Racket syntax checker using the Racket compiler.

See URL `http://racket-lang.org/'."
  :command ("racket" "-f" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes scheme-mode
  :predicate (lambda ()
               (and (buffer-file-name)
                    (string-equal (file-name-extension (buffer-file-name))
                                  "rkt"))))

(add-to-list 'flycheck-checkers 'racket-alt)

(setq flycheck-completion-system 'ido)

;;; C
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "k&r")
                        (other . "gnu")))

(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook 'electric-pair-mode)

;;; Clojure
(let ((cider-loc (expand-directory-name "cider" my-site-lisp-path)))
  (when (file-directory-p cider-loc)
    (add-to-list 'load-path cider-loc)
    (autoload 'cider-connect "cider.el" nil t)
    (autoload 'cider-jack-in "cider.el" nil t)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)))

;;; Haskell
(let ((hstyle (expand-file-name "haskell-style.el" my-site-lisp-path)))
  (when (file-readable-p hstyle)
    (load-file hstyle)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'flycheck-mode)
(add-hook 'haskell-mode-hook 'haskell-style)

;;; Lisp Modes
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'ielm-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

(add-hook 'lisp-mode-hook 'paredit-mode)

;;; SLIME
(let ((ccl-loc (executable-find "ccl"))
      (sbcl-loc (executable-find "sbcl"))
      (slime-loc (expand-directory-name "slime" my-site-lisp-path)))
  (when (file-directory-p slime-loc)
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
      (setq slime-default-lisp 'sbcl))
    (when (boundp 'slime-lisp-implementations)
      (add-to-list 'load-path slime-loc)
      (require 'slime-autoloads)
      (slime-setup '(slime-fancy slime-banner)))))

;;; Nix
(autoload 'nix-mode (expand-file-name "nix-mode.el" my-site-lisp-path)
  "Major mode for editing Nix expressions." t)
(push '("\\.nix\\'" . nix-mode) auto-mode-alist)
(push '("\\.nix.in\\'" . nix-mode) auto-mode-alist)

;;; OCaml
(when (and (file-directory-p "~/.opam") (executable-find "opam"))
  (dolist (var (car (read-from-string
                     (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq exec-path (append (parse-colon-path (getenv "PATH"))
                          (list exec-directory)))
  (let ((ocaml-toplevel-path
         (expand-directory-name "../../share/emacs/site-lisp"
                                (getenv "OCAML_TOPLEVEL_PATH"))))
    (when (file-directory-p ocaml-toplevel-path)
      (add-to-list 'load-path ocaml-toplevel-path))))

(when (and (executable-find "utop") (locate-file "utop.el" load-path))
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer))

;;; Scheme (Racket/Guile)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'flycheck-mode)

(when (file-directory-p (expand-directory-name "geiser" my-site-lisp-path))
  (load-file (expand-file-name "geiser/elisp/geiser.el" my-site-lisp-path)))

(when (executable-find "plt-r5rs")
  (setq scheme-program-name "plt-r5rs"))

;;; Supercollider
(when (and (is-darwin-p)
           (executable-find "sclang")
           (file-directory-p (expand-directory-name "~/src/other/scel")))
  (add-to-list 'load-path (expand-directory-name "~/src/other/scel"))
  (require 'sclang)
  (add-hook 'sclang-mode-hook 'electric-pair-mode))
