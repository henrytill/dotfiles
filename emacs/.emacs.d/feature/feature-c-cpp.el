;;; c-mode & c++-mode

(with-eval-after-load 'cc-styles
  (c-add-style "stevens" '("bsd" (c-basic-offset . 4)))
  (c-add-style "hnf"     '("bsd" (c-basic-offset . 2)))
  (add-to-list 'c-default-style '(c-mode . "stevens"))
  (add-to-list 'c-default-style '(c++-mode . "stroustrup")))

(use-package c-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode))
  :init
  (add-hook 'c-mode-hook 'electric-pair-mode))

(use-package c++-mode
  :mode (("\\.cc\\'"  . c++-mode)
         ("\\.cpp\\'" . c++-mode))
  :init
  (add-hook 'c++-mode-hook 'electric-pair-mode))


;;; rtags

(eval-and-compile
  (defun ht/rtags-load-path ()
    (let ((rdm-executable (executable-find "rdm")))
      (when rdm-executable
        (expand-directory-name "../share/emacs/site-lisp/rtags"
                               (file-name-directory rdm-executable))))))

(defun ht/flycheck-rtags-mode ()
  (flycheck-mode 1)
  (when (not (featurep 'flycheck-rtags))
    (require 'flycheck-rtags))
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil))

(defun ht/rtags-mode ()
  (rtags-start-process-unless-running)
  (ht/flycheck-rtags-mode))

(use-package rtags
  :if (executable-find "rdm")
  :defines rtags-start-process-unless-running
  :commands rtags-start-process-unless-running
  :load-path (lambda () (ht/rtags-load-path))
  :init
  (setq rtags-autostart-diagnostics t
        rtags-completions-enabled t)
  (add-hook 'c-mode-hook #'ht/rtags-mode)
  (add-hook 'c++-mode-hook #'ht/rtags-mode)
  :config
  (when (not (featurep 'company-rtags))
    (require 'company-rtags)
    (add-to-list 'company-backends 'company-rtags))
  (bind-map ht/rtags-leader-map
    :keys ("M-m")
    :evil-keys ("SPC")
    :evil-states (motion normal visual paredit)
    :major-modes (c++-mode))
  (bind-map-set-keys ht/rtags-leader-map
    "t" 'rtags-symbol-type)
  (evil-define-key 'normal c++-mode-map (kbd "C-c C-a") 'ff-find-other-file)
  (evil-define-key 'normal c++-mode-map (kbd "C-]") 'rtags-find-symbol-at-point)
  (evil-define-key 'normal c++-mode-map (kbd "C-t") 'rtags-location-stack-back))


;;; clang-format

(use-package clang-format
  :ensure t
  :commands (clang-format
             clang-format-buffer
             clang-format-region)
  :config
  (when (string-equal "nereus" (ht/hostname))
    (setq clang-format-executable "/opt/clang+llvm-3.9.0-x86_64-apple-darwin/bin/clang-format")))

(provide 'feature-c-cpp)
