;;; c-mode & c++-mode

(with-eval-after-load 'cc-styles
  (c-add-style "bsd-tabs" '("bsd" (indent-tabs-mode . t)))
  (c-add-style "hcpp"     '((indent-tabs-mode . nil)
                            (c-basic-offset . 4)
                            (c-offsets-alist (statement-block-intro . +)
                                             (knr-argdecl-intro . 0)
                                             (substatement-open . 0)
                                             (substatement-label . 0)
                                             (label . 0)
                                             (statement-cont . +)
                                             (innamespace . 0))))
  (add-to-list 'c-default-style '(c-mode . "bsd-tabs"))
  (add-to-list 'c-default-style '(c++-mode . "hcpp")))

(add-hook 'c-mode-hook   #'electric-pair-mode)
(add-hook 'c++-mode-hook #'electric-pair-mode)


;;; clang-format

(use-package clang-format
  :ensure t
  :commands (clang-format
             clang-format-buffer
             clang-format-region))

(provide 'feature-c-cpp)
