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
              org-babel-ocaml-command    (format "ocaml -init %s"       ocamlinit)
              tuareg-interactive-program (format "ocaml -init %s"       ocamlinit)
              utop-command               (format "utop -emacs -init %s" ocamlinit))))))

(defun ht/merlin-mode ()
  (let ((extension (file-name-extension buffer-file-name)))
    (when (not (or (string-equal "mll" extension)
                   (string-equal "mly" extension)))
      (bind-map ht/merlin-leader-map
        :keys ("M-m")
        :evil-keys ("SPC")
        :evil-states (motion normal visual paredit)
        :minor-modes (merlin-mode))
      (bind-map-set-keys ht/merlin-leader-map
        "t" 'merlin-type-enclosing)
      (evil-define-key 'normal merlin-mode-map "gd"  'merlin-locate)
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
  (bind-map ht/tuareg-leader-map
    :keys ("M-m c")
    :evil-keys ("SPC c")
    :evil-states (motion normal visual paredit)
    :major-modes (tuareg-mode))
  (bind-map-set-keys ht/tuareg-leader-map
    "a" 'tuareg-find-alternate-file)
  (electric-indent-mode 0)
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
    :init
    (add-hook 'tuareg-mode-hook #'ht/merlin-mode))
  (use-package utop
    :if (and (executable-find "utop")
             (locate-file "utop.el" load-path))
    :commands (utop utop-minor-mode)
    :init
    (add-hook 'tuareg-mode-hook 'utop-minor-mode))
  (use-package ocp-indent
    :if (and (executable-find "ocp-indent")
             (locate-file "ocp-indent.el" load-path))
    :commands ocp-setup-indent
    :init
    (add-hook 'tuareg-mode-hook 'ocp-setup-indent))
  (dolist (mode '(electric-pair-mode
                  ht/tuareg-mode))
    (add-hook 'tuareg-mode-hook mode)))

(use-package sml-mode
  :ensure t
  :mode "\\.sml\\'")

(provide 'feature-ml)
