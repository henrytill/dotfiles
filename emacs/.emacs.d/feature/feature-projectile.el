(use-package projectile
  :load-path "site-lisp/projectile"
  :functions (projectile-global-mode
              projectile-switch-project
              projectile-find-file
              projectile-kill-buffers)
  :commands (projectile-switch-project
             projectile-find-file
             projectile-kill-buffers)
  :bind (("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file)
         ("C-c p k" . projectile-kill-buffers))
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (add-to-list 'projectile-project-root-files "_tags")
  (projectile-register-project-type 'ocamlbuild-make
                                    '("_tags" "Makefile")
                                    :compile "make"
                                    :test "make test")
  (projectile-register-project-type 'ocamlbuild-script
                                    '("_tags" "build")
                                    :compile "./build"
                                    :test "./build tests")
  (projectile-register-project-type 'topkg
                                    '("_tags" "opam" "pkg")
                                    :compile "topkg build"
                                    :test "topkg test"))

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
