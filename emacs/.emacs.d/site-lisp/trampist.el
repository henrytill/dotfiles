;;; trampist.el --- Tramp setup for the podman devcontainer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Henry Till

;; Author: Henry Till <henrytill@gmail.com>
;; Keywords: comm, processes

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tramp's connection shell is deliberately init-free: the podman method runs
;; "podman exec -it -u USER HOST /bin/sh -i", which is interactive but not a
;; login shell, so nothing in /etc/profile.d is sourced.  The container's
;; entrypoint does not run for exec either.  Everything here exists to put
;; back, selectively, the parts of that environment we actually want.
;;
;; Loading this file registers a connection-local profile for the podman
;; method: bash as the remote shell, and a `tramp-remote-process-environment'
;; built from `ht/devcontainer-base-env'.  That much is static, and enough for
;; a shell that behaves.
;;
;; The toolchain needs more.  `ht/devcontainer-sync-env' lifts the variables
;; named in `ht/devcontainer-env-imports' out of a real login shell in the
;; container and merges them into the profile, so that M-x compile and
;; friends see what an interactive session there would see.  Each value
;; embeds the current opam switch, so re-run it after `opam switch'.
;;
;; The probe runs podman directly rather than going over Tramp, which lets it
;; work before any connection exists -- and as the same user Tramp would
;; connect as, since HOME, and so which ~/.profile runs, follows from that.

;;; Code:

;; both live in tramp-loaddefs.el, so (require 'tramp) is enough to bind
;; them — tramp-container.el itself need not be loaded
(defvar tramp-podman-method)
(defvar tramp-podman-program)

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

;; Loading this file unconditionally is deliberate: it mirrors Tramp, which
;; registers the podman method whether or not podman is installed and shells
;; out only when a connection or a completion actually asks it to.  Nothing
;; here runs a program at load time, so there is no executable-find guard;
;; the two commands below use `tramp-podman-program' so a renamed or wrapped
;; client stays a single point of configuration.

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
   `(:application tramp :protocol ,tramp-podman-method) 'ht/devcontainer-env))

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
  (require 'tramp)
  (with-temp-buffer
    (unless (zerop (call-process tramp-podman-program nil t nil
                                 "ps" "--format" "{{.Names}}"))
      (error "Cannot list containers: %s" (string-trim (buffer-string))))
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
                          tramp-podman-program nil t nil
                          `("exec"
                            ,@(when-let* ((user (tramp-file-name-user vec)))
                                (list "-u" user))
                            ,(tramp-file-name-host vec)
                            "bash" "-lc" "env -0")))
      (error "Cannot read environment from %s: %s"
             (tramp-file-name-host vec) (string-trim (buffer-string))))
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
CONTAINER is a name from `ht/devcontainer-containers'.  From Lisp it may
also carry a \"USER@\" prefix, naming the connection as Tramp addresses
it; interactively the prompt requires a match against the running
containers, so a typo cannot be taken for a container that is simply not
running yet.
The imported values reach remote processes through the environment Tramp
exports once at connection setup, so any existing connection is flushed;
the next remote operation reconnects with the new values.  Re-run after
`opam switch', which invalidates every imported path."
  (interactive (list (completing-read "Container: "
                                      (ht/devcontainer-containers) nil t)))
  ;; deferred rather than a top-level `require': loading this file must not
  ;; drag in Tramp at startup, but nothing below works without it
  (require 'tramp)
  ;; one parse drives both the probe and the flush, so they cannot disagree
  ;; about which user's connection is being refreshed
  (let* ((vec (tramp-dissect-file-name
               (format "/%s:%s:/" tramp-podman-method container)))
         (imported (ht/devcontainer-login-env vec))
         ;; shadow only what actually came back, not every name we asked
         ;; for: a variable the login shell did not export must keep its
         ;; static fallback from `ht/devcontainer-base-env' rather than
         ;; end up unset, which would make syncing worse than not syncing
         (shadowed (mapcar (lambda (entry) (car (split-string entry "=")))
                           imported)))
    (connection-local-update-profile-variables
     'ht/devcontainer-env
     `((tramp-remote-process-environment
        . ,(append (seq-remove
                    (lambda (entry)
                      (member (car (split-string entry "=")) shadowed))
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
