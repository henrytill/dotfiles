;;;; scheming.el -- various scheme-related functions

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

(add-hook 'scheme-mode-hook 'my-scheme-complete)
(add-hook 'scheme-mode-hook 'my-scheme-doc)

(add-hook 'inferior-scheme-mode-hook 'my-scheme-complete)
(add-hook 'inferior-scheme-mode-hook 'my-scheme-doc)

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

(eval-after-load 'geiser-mode
  '(progn (defun geiser-run-tests ()
            (interactive)
            (cond ((string-equal geiser-impl--implementation "racket")
                   (progn (when (string-equal major-mode "scheme-mode")
                            (geiser-mode-switch-to-repl-and-enter))
                          (insert ",enter #f ")
                          (geiser-repl--maybe-send)
                          (insert "(require test-engine/racket-tests)\n")
                          (insert "(test)")
                          (geiser-repl--maybe-send)))
                  (t nil)))))
