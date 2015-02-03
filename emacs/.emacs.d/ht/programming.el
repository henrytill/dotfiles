;;;; Programming Modes

;;; Flycheck
(global-flycheck-mode 0)

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
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'flycheck-mode)

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
(autoload 'nix-mode (expand-file-name "nix-mode.el" my-site-lisp-path) "Major mode for editing Nix expressions." t)
(push '("\\.nix\\'" . nix-mode) auto-mode-alist)
(push '("\\.nix.in\\'" . nix-mode) auto-mode-alist)

;;; OCaml
(when (and (file-directory-p "~/.opam") (executable-find "opam"))
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq exec-path (append (parse-colon-path (getenv "PATH"))
                          (list exec-directory)))
  (when (file-directory-p (expand-directory-name "../../share/emacs/site-lisp"
                                                 (getenv "OCAML_TOPLEVEL_PATH")))
    (add-to-list 'load-path (expand-directory-name "../../share/emacs/site-lisp"
                                                   (getenv "OCAML_TOPLEVEL_PATH")))))

(when (and (executable-find "utop") (locate-file "utop.el" load-path))
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer))

;;; Scheme
(defvar my-schemes-alist nil)

(setq my-schemes-alist '(("chez" . "petite")
                         ("chibi" . "chibi-scheme")
                         ("chicken" . "csi -:c")
                         ("gambit" . "gsi -:d-")
                         ("kawa" . "kawa")
                         ("plt-r5rs" . "plt-r5rs")
                         ("vicare" . "vicare")))

(cond  ((executable-find "petite") (setq scheme-program-name "petite"))
       ((executable-find "vicare") (setq scheme-program-name "vicare"))
       ((executable-find "csi") (setq scheme-program-name "csi -:c"))
       ((executable-find "gsi") (setq scheme-program-name "gsi -:d-"))
       ((executable-find "plt-r5rs") (setq scheme-program-name "plt-r5rs"))
       (t nil))

(defun which-scheme (name)
  (interactive
   (let ((installed (delq nil (mapcar (lambda (element) (and (shell-command-p (cdr element))
                                                             (car element)))
                                      my-schemes-alist))))
     (list (completing-read "Which Scheme interpreter to run? " installed))))
  (if (assoc name my-schemes-alist)
      (setq scheme-program-name (cdr (assoc name my-schemes-alist)))
    (setq scheme-program-name name))
  (run-scheme scheme-program-name))

(defun my-scheme-complete ()
  (autoload 'scheme-smart-complete
    (expand-file-name "scheme-complete-0.8.11.el.gz" my-site-lisp-path) nil t)
  (define-key scheme-mode-map "\e\t" 'scheme-smart-complete))

(defun my-scheme-doc ()
  (cond ((equal scheme-program-name "csi -:c")
         (defun chicken-doc (&optional obtain-function)
           (interactive)
           (let ((func (funcall (or obtain-function 'current-word))))
             (when func
               (process-send-string (scheme-proc)
                                    (format "(require-library chicken-doc) ,doc %S\n" func))
               (save-selected-window
                 (select-window (display-buffer (get-buffer scheme-buffer) t))
                 (goto-char (point-max))))))
         (define-key scheme-mode-map "\C-cd"
           (lambda () (interactive) (chicken-doc 'sexp-at-point))))
        (t (fmakunbound 'chicken-doc)
           (define-key scheme-mode-map "\C-cd" nil)))
  (autoload 'scheme-get-current-symbol-info
    (expand-file-name "scheme-complete-0.8.11.el.gz" my-site-lisp-path) nil t)
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
  (eldoc-mode))

(defun scheme-mode-reload ()
  (interactive)
  (add-hook 'scheme-mode-hook 'my-scheme-complete)
  (add-hook 'scheme-mode-hook 'my-scheme-doc)
  (scheme-mode))

(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'flycheck-mode)

(add-hook 'inferior-scheme-mode-hook 'my-scheme-complete)
(add-hook 'inferior-scheme-mode-hook 'my-scheme-doc)

;;; Geiser
(when (file-directory-p (expand-directory-name "geiser" my-site-lisp-path))
  (defun load-geiser ()
    (interactive)
    (when (functionp 'chicken-doc)
      (fmakunbound 'chicken-doc)
      (define-key scheme-mode-map "\C-cd" nil))
    (when (boundp 'scheme-mode-map)
      (define-key scheme-mode-map "\e\t" nil))
    (remove-hook 'scheme-mode-hook 'my-scheme-doc)
    (remove-hook 'scheme-mode-hook 'my-scheme-complete)
    (load-file (expand-file-name "geiser/elisp/geiser.el" my-site-lisp-path))
    (if (y-or-n-p "Start REPL now? ")
        (call-interactively 'run-geiser)
      (message nil))))

;;; Supercollider
(when (and (is-darwin-p)
           (executable-find "sclang")
           (file-directory-p (expand-directory-name "~/src/other/scel")))
  (add-to-list 'load-path (expand-directory-name "~/src/other/scel"))
  (require 'sclang)
  (add-hook 'sclang-mode-hook 'electric-pair-mode))
