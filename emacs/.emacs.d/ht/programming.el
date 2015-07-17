;;;; Programming Modes

(add-hook 'prog-mode-hook 'ht-add-watchwords)
(add-hook 'prog-mode-hook 'page-break-lines-mode)
(add-hook 'prog-mode-hook 'undo-tree-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)

;;; Warning Keywords
(defun ht-add-watchwords ()
  (font-lock-add-keywords nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\)" 1 font-lock-warning-face t))))

;;; page-break-lines
(setq page-break-lines-char ?-)

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
    (require 'cl)
    (require 'cider)
    (require 'cider-apropos)
    (require 'cider-browse-ns)
    (require 'cider-classpath)
    (require 'cider-grimoire)
    (require 'cider-inspector)
    (require 'cider-macroexpansion)
    (require 'cider-scratch)
    (require 'cider-selector)
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (remove-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
    (setq cider-show-error-buffer 'except-in-repl)))

(add-hook 'clojure-mode-hook 'paredit-mode)

(defun ht-revert-clojure-buffer ()
  (let ((ext (file-name-extension buffer-file-name)))
    (when (or (string-equal ext "clj")
              (string-equal ext "cljs")
              (string-equal ext "cljc"))
      (revert-buffer))))

(defun ht-load-inf-clojure ()
  (interactive)
  (setq cider-auto-mode nil)
  (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
  (ht-revert-clojure-buffer))

(defun ht-unload-inf-clojure ()
  (interactive)
  (remove-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
  (setq cider-auto-mode t)
  (ht-revert-clojure-buffer))

(setq inf-clojure-program "lein trampoline run -m clojure.main")

;;; Haskell
(let ((hstyle (expand-file-name "haskell-style.el" my-site-lisp-path)))
  (when (file-readable-p hstyle)
    (load-file hstyle)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-style)

;;; Lisp Modes
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'ielm-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

(add-hook 'lisp-mode-hook 'paredit-mode)

;;; Quicklisp-related
(let* ((quicklisp-loc (expand-directory-name "~/quicklisp"))
       (clhs-use-local (expand-file-name "clhs-use-local.el" quicklisp-loc)))
  ;; Local HyperSpec
  (when (file-exists-p clhs-use-local)
    (load-file clhs-use-local)))

;;; SLIME
(let ((ccl-loc (executable-find "ccl64"))
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
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (let ((ocaml-toplevel-path (expand-directory-name "../../share/emacs/site-lisp"
                                                    (getenv "OCAML_TOPLEVEL_PATH"))))
    (when (file-directory-p ocaml-toplevel-path)
      (add-to-list 'load-path ocaml-toplevel-path))))

(when (and (executable-find "utop") (locate-file "utop.el" load-path))
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer))

;;; Scheme (Racket/Guile)
(add-hook 'scheme-mode-hook 'paredit-mode)

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
