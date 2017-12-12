;;; c-mode & c++-mode

(with-eval-after-load 'cc-styles
  (c-add-style "stevens" '("bsd" (c-basic-offset . 4)))
  (c-add-style "hnf"     '("bsd" (c-basic-offset . 2)))
  (add-to-list 'c-default-style '(c-mode . "bsd"))
  (add-to-list 'c-default-style '(c++-mode . "stroustrup")))

(defun ht/c-mode ()
  (setq indent-tabs-mode t))

(use-package c-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode))
  :init
  (add-hook 'c-mode-hook #'ht/c-mode)
  (add-hook 'c-mode-hook #'electric-pair-mode))

(use-package c++-mode
  :mode (("\\.cc\\'"  . c++-mode)
         ("\\.hh\\'"  . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :init
  (add-hook 'c++-mode-hook #'electric-pair-mode))


;;; rtags

(eval-and-compile
  (defun ht/rtags-load-path ()
    (let ((rdm-executable (executable-find "rdm")))
      (when rdm-executable
        (expand-directory-name "../share/emacs/site-lisp/rtags"
                               (file-name-directory rdm-executable))))))

(defun ht/flycheck-rtags-mode ()
  (interactive)
  (flycheck-mode 1)
  (when (not (featurep 'flycheck-rtags))
    (require 'flycheck-rtags))
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil))

(defun ht/rtags-mode ()
  (setq rtags-autostart-diagnostics t
        rtags-completions-enabled t)
  (rtags-start-process-unless-running))

(use-package rtags
  :if (executable-find "rdm")
  :defines rtags-start-process-unless-running
  :commands rtags-start-process-unless-running
  :load-path (lambda () (ht/rtags-load-path))
  :init
  (add-hook 'c-mode-hook   #'ht/rtags-mode)
  (add-hook 'c++-mode-hook #'ht/rtags-mode)
  :config
  (when (not (featurep 'company-rtags))
    (require 'company-rtags)
    (add-to-list 'company-backends 'company-rtags))
  (dolist (map '(c-mode-map c++-mode-map))
    (dolist (binding '(("C-c C-a" . ff-find-other-file)
                       ("C-]"     . rtags-find-symbol-at-point)
                       ("C-t"     . rtags-location-stack-back)))
      (eval `(evil-define-key 'normal ,map (kbd (car binding)) (cdr binding))))))

(defhydra ht/hydra-rtags (:idle 1)
  "
rtags
-----
_t_: rtags-symbol-type
"
  ("t" rtags-symbol-type nil :exit t))


;;; clang-format

(use-package clang-format
  :ensure t
  :commands (clang-format
             clang-format-buffer
             clang-format-region))

(provide 'feature-c-cpp)
