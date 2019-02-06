(use-package projectile
  :ensure t
  :functions (projectile-global-mode
              projectile-switch-project
              projectile-find-file
              projectile-kill-buffers)
  :commands (projectile-switch-project
             projectile-find-file
             projectile-kill-buffers)
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'ivy)

  (projectile-global-mode)

  (add-to-list 'projectile-project-root-files "_tags")

  (defun projectile-cabal-new-project-p ()
    "Check if a project contains a cabal.project.local."
    (projectile-verify-file "cabal.project.local"))

  (projectile-register-project-type 'haskell-cabal-new
                                    #'projectile-cabal-new-project-p
                                    :compile "cabal new-build"
                                    :test "cabal new-test")
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
                                    :test "topkg test")

  ;; https://github.com/joaotavora/eglot/issues/129
  (defun ht/projectile-project-find-function (dir)
    (let ((projectile-root (projectile-project-root dir)))
      (and projectile-root (cons 'transient projectile-root))))

  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'ht/projectile-project-find-function)))

(defhydra ht/hydra-projectile-search (:idle 1.5)
  "
projectile-search
-----------------
_g_: projectile-grep
"
  ("g" projectile-grep nil :exit t))

(defhydra ht/hydra-projectile (:idle 1.5)
  "
projectile
----------
_p_: projectile-switch-project
_c_: projectile-compile-project
_t_: projectile-test-project
_f_: projectile-find-file
_s_: ht/hydra-projectile-search/body
_b_: projectile-switch-to-buffer
_k_: projectile-kill-buffers
"
  ("p" projectile-switch-project       nil :exit t)
  ("c" projectile-compile-project      nil :exit t)
  ("t" projectile-test-project         nil :exit t)
  ("f" projectile-find-file            nil :exit t)
  ("r" projectile-find-file            nil :exit t)
  ("s" ht/hydra-projectile-search/body nil :exit t)
  ("b" projectile-switch-to-buffer     nil :exit t)
  ("k" projectile-kill-buffers         nil :exit t))

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
