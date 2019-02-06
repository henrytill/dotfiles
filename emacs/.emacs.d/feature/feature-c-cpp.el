;;; c-mode & c++-mode

(with-eval-after-load 'cc-styles
  (c-add-style "k&r-4"   '("k&r" (c-basic-offset . 4)))
  (c-add-style "stevens" '("bsd" (c-basic-offset . 4)))
  (c-add-style "hnf"     '("bsd" (c-basic-offset . 2)))
  (c-add-style "hnfcpp"  '("stroustrup" (c-basic-offset . 2)))
  (add-to-list 'c-default-style '(c-mode . "k&r-4"))
  (add-to-list 'c-default-style '(c++-mode . "hnfcpp")))

(add-hook 'c-mode-hook   #'electric-pair-mode)
(add-hook 'c++-mode-hook #'electric-pair-mode)


;;; clang-format

(use-package clang-format
  :ensure t
  :commands (clang-format
             clang-format-buffer
             clang-format-region))

(provide 'feature-c-cpp)
