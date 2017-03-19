(defun ht/company-coq-fix-issue-126 ()
  "https://github.com/cpitclaudel/company-coq/issues/126"
  (defconst company-coq-tg--preprocessor-substitutions
    '(("\n"  . " ")
      ("[ "  . "( OR-GROUP ")
      (" ]"  . " )")
      (" | " . " OR ")
      ("; "  . " AND ")
      ("'"   . "’"))))

(with-eval-after-load 'proof-faces
  (set-face-attribute 'proof-locked-face nil :background "#222232"))

(use-package proof-site
  :load-path "site-lisp/PG/generic"
  :mode ("\\.v\\'" . coq-mode)
  :init
  (use-package coq-mode
    :commands coq-mode
    :init
    (use-package company-coq
      :ensure t
      :defines company-coq-disabled-features
      :diminish company-coq-mode
      :commands company-coq-mode
      :init
      (setq company-coq-disabled-features '(prettify-symbols
                                            smart-subscripts))
      (add-hook 'company-coq-mode-hook #'ht/company-coq-fix-issue-126))
    (add-hook 'coq-mode-hook 'company-coq-mode)
    (add-hook 'coq-mode-hook 'electric-pair-mode)
    (add-hook 'coq-mode-hook 'whitespace-mode)))

(provide 'feature-coq)
