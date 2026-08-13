;;; trampist.el --- Tramp setup for the podman devcontainer -*- lexical-binding: t -*-

;; Tramp's connection shell is deliberately init-free: the podman method runs
;; "podman exec -it -u USER HOST /bin/sh -i", which is interactive but not a
;; login shell, so nothing in /etc/profile.d is sourced.  The container's
;; entrypoint does not run for exec either.  Everything below exists to put
;; back, selectively, the parts of that environment we actually want.

(defconst ht/devcontainer-base-env
  '("ENV=''" "TMOUT=0" "LC_CTYPE=en_US.UTF-8"
    "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=cat"
    "autocorrect=" "correct="
    ;; static fallback; `ht/devcontainer-sync-env' replaces it with the
    ;; value a login shell in the container actually reports
    "SSH_AUTH_SOCK=/run/user/1000/keyring/ssh")
  "Base value for `tramp-remote-process-environment' in the container.
This is the default from tramp.el with LC_CTYPE given a real locale:
Tramp ships it as the literal two-apostrophe string, which bash rejects
with a setlocale warning once `shell-file-name' is bash rather than sh.")

;; safe at init time — just defines the bundle
(connection-local-set-profile-variables
 'ht/devcontainer-env
 `((shell-file-name . "/bin/bash")
   (shell-command-switch . "-c")
   (explicit-bash-args . ("--noediting" "-l" "-i"))
   (tramp-remote-process-environment . ,ht/devcontainer-base-env)))

;; must run AFTER tramp-integration registers its defaults: new criteria are
;; consed onto the front of `connection-local-criteria-alist' and the resolver
;; takes the first definition it finds, so the last registration wins.  The
;; (require 'shell) is load-bearing — Tramp registers its own shell profile
;; from a `with-eval-after-load' on shell, and that must land before ours.
(with-eval-after-load 'tramp
  (require 'shell)
  (connection-local-set-profiles
   '(:application tramp :protocol "podman") 'ht/devcontainer-env))

;;; Importing the login-shell environment

;; Compile-time only; none of these are autoloaded outside tramp.el, so the
;; commands below `require' Tramp before calling them.
(declare-function tramp-cleanup-connection "tramp-cmds")
(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-file-name-host "tramp")
(declare-function tramp-file-name-user "tramp")

;; Variables set by ~/.profile (opam's init hook, the ssh-agent drop-in) are
;; invisible to Tramp, which matters most for M-x compile: without
;; CAML_LD_LIBRARY_PATH and friends the toolchain half-works.  Hardcoding them
;; would rot on every `opam switch', since each value embeds the switch name.
;;
;; PATH is deliberately absent and must stay absent: it is forbidden in
;; `tramp-remote-process-environment'.  Use `tramp-remote-path' (with
;; `tramp-own-remote-path') if Tramp ever fails to find a remote executable.

(defvar ht/devcontainer-env-imports
  '("SSH_AUTH_SOCK"
    "OPAM_SWITCH_PREFIX" "CAML_LD_LIBRARY_PATH"
    "OCAML_TOPLEVEL_PATH" "OCAMLTOP_INCLUDE_PATH" "MANPATH")
  "Variables to lift out of a login shell inside the container.")

(defun ht/devcontainer-containers ()
  "Return the names of the running podman containers."
  (with-temp-buffer
    (unless (zerop (call-process "podman" nil t nil
                                 "ps" "--format" "{{.Names}}"))
      (error "podman ps failed: %s" (string-trim (buffer-string))))
    (split-string (buffer-string) "\n" t)))

(defun ht/devcontainer-login-env (vec)
  "Return (\"VAR=VAL\" ...) for `ht/devcontainer-env-imports' in VEC.
VEC is a dissected Tramp file name naming the container.  Queried with
podman directly rather than over Tramp, so this can run before any
connection exists -- but as the user Tramp itself would connect as,
since HOME, and therefore which ~/.profile runs, depends on it.  A nil
user means the image's default: Tramp drops its (\"-u\" \"%u\") login-arg
group when the expansion is empty, so we omit -u for the same case."
  (with-temp-buffer
    (unless (zerop (apply #'call-process
                          "podman" nil t nil
                          `("exec"
                            ,@(when-let* ((user (tramp-file-name-user vec)))
                                (list "-u" user))
                            ,(tramp-file-name-host vec)
                            "bash" "-lc" "env -0")))
      (error "podman exec failed: %s" (string-trim (buffer-string))))
    (delq nil
          (mapcar (lambda (entry)
                    (let ((name (car (split-string entry "="))))
                      (and (member name ht/devcontainer-env-imports)
                           (not (string-empty-p
                                 (substring entry (1+ (length name)))))
                           entry)))
                  (split-string (buffer-string) "\0" t)))))

(defun ht/devcontainer-sync-env (container)
  "Merge CONTAINER's login-shell environment into the connection profile.
CONTAINER is a name from `ht/devcontainer-containers', optionally
prefixed with \"USER@\" to match how the connection is addressed.
The imported values reach remote processes through the environment Tramp
exports once at connection setup, so any existing connection is flushed;
the next remote operation reconnects with the new values.  Re-run after
`opam switch', which invalidates every imported path."
  (interactive (list (completing-read "Container: "
                                      (ht/devcontainer-containers)
                                      nil 'confirm)))
  ;; deferred rather than a top-level `require': loading this file must not
  ;; drag in Tramp at startup, but nothing below works without it
  (require 'tramp)
  ;; one parse drives both the probe and the flush, so they cannot disagree
  ;; about which user's connection is being refreshed
  (let* ((vec (tramp-dissect-file-name (format "/podman:%s:/" container)))
         (imported (ht/devcontainer-login-env vec)))
    (connection-local-update-profile-variables
     'ht/devcontainer-env
     `((tramp-remote-process-environment
        . ,(append (seq-remove
                    (lambda (entry)
                      (member (car (split-string entry "="))
                              ht/devcontainer-env-imports))
                    ht/devcontainer-base-env)
                   imported))))
    ;; a never-connected vector is fine here: the cleanup is a no-op
    (tramp-cleanup-connection
     vec
     ;; keep async processes: existing shell buffers survive the flush
     'keep-debug 'keep-password 'keep-processes)
    (message "Imported %d variable(s) from %s" (length imported) container)
    imported))

(provide 'trampist)
;;; trampist.el ends here
