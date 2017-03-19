(use-package projectile
  :load-path "site-lisp/projectile"
  :defer 5
  :functions projectile-global-mode
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (add-to-list 'projectile-project-root-files "_tags")
  (projectile-register-project-type 'ocamlbuild-make '("_tags" "Makefile") "make" "make test")
  (projectile-register-project-type 'ocamlbuild-script '("_tags" "build") "./build" "./build tests"))

(provide 'feature-projectile)
