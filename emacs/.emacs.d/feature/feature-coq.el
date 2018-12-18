(defun ht/coq-mode ()
  (setq-local overlay-arrow-string ""))

(defun ht/company-coq-fix-issue-126 ()
  "https://github.com/cpitclaudel/company-coq/issues/126"
  (defconst company-coq-tg--preprocessor-substitutions
    '(("\n"  . " ")
      ("[ "  . "( OR-GROUP ")
      (" ]"  . " )")
      (" | " . " OR ")
      ("; "  . " AND ")
      ("'"   . "’"))))

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
      :commands company-coq-mode
      :init
      (setq company-coq-disabled-features '(prettify-symbols
                                            smart-subscripts)))
    (dolist (mode '(company-coq-mode
                    electric-pair-mode
                    ht/coq-mode
                    whitespace-mode))
      (add-hook 'coq-mode-hook mode))))

(provide 'feature-coq)
