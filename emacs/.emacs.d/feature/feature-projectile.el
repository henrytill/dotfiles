(use-package projectile
  :load-path "site-lisp/projectile"
  :defer 5
  :functions projectile-global-mode
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (add-to-list 'projectile-project-root-files "_tags")
  (projectile-register-project-type 'ocamlbuild-make '("_tags" "Makefile") "make" "make test")
  (projectile-register-project-type 'ocamlbuild-script '("_tags" "build") "./build" "./build tests")
  (projectile-register-project-type 'topkg '("_tags" "opam" "pkg") "topkg build" "topkg test"))

(with-eval-after-load 'projectile
  ;; Redefine to observe projectile-compilation-dir
  (defun projectile-test-project (arg &optional dir)
    "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
    (interactive "P")
    (let* ((project-root (projectile-project-root))
           (default-directory (or dir (projectile-compilation-dir)))
           (default-cmd (projectile-test-command default-directory))
           (test-cmd (projectile-maybe-read-command arg default-cmd "Test command: ")))
      (puthash default-directory test-cmd projectile-test-cmd-map)
      (save-some-buffers (not compilation-ask-about-save)
                         (lambda ()
                           (projectile-project-buffer-p (current-buffer)
                                                        project-root)))
      (projectile-run-compilation test-cmd))))

(provide 'feature-projectile)
