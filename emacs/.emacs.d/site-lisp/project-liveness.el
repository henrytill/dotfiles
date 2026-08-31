;;; project-liveness.el --- Prune dead entries from project.el's known list  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Henry Till

;; Author: Henry Till <henrytill@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: project, convenience, tramp

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

;; `project-forget-zombie-projects' decides whether a known project is
;; still around by calling `file-exists-p' on its root.  For roots on
;; remote hosts and in containers that is a poor test: it opens a Tramp
;; connection, so it hangs on an unreachable host, and it cannot tell
;; "the project was deleted" apart from "the container is stopped" --
;; both look like a missing directory, and the entry is dropped either
;; way.
;;
;; This library replaces the existence test with a three-valued one:
;; `exists', `missing', or `unknown'.  Only `missing' causes an entry to
;; be forgotten; `unknown' leaves the list untouched.  Podman and Docker
;; roots are resolved by asking the container engine directly, without any
;; Tramp connection.
;;
;; Entry points:
;;
;;   M-x project-liveness-forget-dead-projects   (C-u for a dry run)
;;   M-x project-liveness-list-dead-projects
;;
;; Emacs 28.1 or later.  Tramp is deliberately not required at load time:
;; `file-remote-p' is in files.el, and `tramp-podman-program' and
;; `tramp-docker-program' are only read if they happen to be bound.
;;
;;
;; Emacs 31 and later
;; ------------------
;;
;; Emacs 31 grows a `project-prune-zombie-projects' user option: an alist
;; of (WHEN . PREDICATE) whose keys say when project.el prunes on its own
;; -- `list-first-read', `list-write', `prompt', or `interactively'.  It
;; defaults to ((prompt . project-prune-zombies-default)), and that
;; predicate is (not (file-remote-p project)), so out of the box only
;; local roots are ever pruned automatically.
;;
;; PREDICATE does not answer the question, it only narrows it.
;; `project--delete-zombie-projects' forgets a root when the predicate
;; passes *and* project.el's own test agrees:
;;
;;   (and (funcall predicate root) (not (file-exists-p root)))
;;
;; So `project-liveness-zombie-p' fits the slot --
;;
;;   (setopt project-prune-zombie-projects
;;           '((prompt . project-liveness-zombie-p)))
;;
;; -- but be clear about what that buys.  It makes container roots
;; eligible for automatic pruning and narrows them to the ones podman or
;; docker says are gone; it does not spare the survivors `file-exists-p',
;; which is the very test this library exists to replace.  Each one is
;; still dialed once.  Emacs 31 at least asks "Forget unreachable
;; project?" when that connection fails, where Emacs 30 drops the entry
;; without asking.  Leaving the option alone and running the commands
;; above keeps container roots out of `file-exists-p' altogether.
;;
;; One interaction to know about: `project--write-project-list' honors
;; the `list-write' key, and `project-liveness--forget' calls it.  An
;; entry under that key turns every forget by this library into a
;; project.el sweep as well.  The default alist has no such entry.
;;
;; Emacs 31 also fixes, upstream, the lookup bug that
;; `project-liveness--forget' works around: there
;; `project--remove-from-project-list' matches its argument literally,
;; which is what this library does on every version.

;;; Code:

(require 'project)
(require 'seq)
(require 'subr-x)

(defgroup project-liveness nil
  "Pruning of stale entries from the known-project list."
  :group 'project
  :prefix "project-liveness-")

(defcustom project-liveness-container-methods
  '(("podman" . podman) ("podmancp" . podman)
    ("docker" . docker) ("dockercp" . docker))
  "Alist mapping a Tramp method to the engine that owns its containers.
The host component of a root using one of these methods names a
container, which is asked about with that engine's command-line client
rather than over Tramp."
  :type '(alist :key-type (string :tag "Tramp method")
                :value-type (choice (const podman) (const docker))))

(defcustom project-liveness-podman-program nil
  "Podman executable used for liveness checks.
When nil, use `tramp-podman-program' if Tramp has been loaded, and
fall back to \"podman\"."
  :type '(choice (const :tag "Auto" nil) string))

(defcustom project-liveness-docker-program nil
  "Docker executable used for liveness checks.
When nil, use `tramp-docker-program' if Tramp has been loaded, and
fall back to \"docker\"."
  :type '(choice (const :tag "Auto" nil) string))

(defcustom project-liveness-remote-policy 'connected-only
  "How to judge remote roots that are not container roots.

`connected-only' -- test the root only when Tramp already has a live
connection to it, so a check never dials out.  This is the safe
default: hosts that are merely down keep their entries.

`always' -- test every remote root, opening connections as needed.
Can block, and can prompt for credentials.

`never' -- never forget a non-container remote root."
  :type '(choice (const connected-only) (const always) (const never)))

(defcustom project-liveness-check-container-path nil
  "When non-nil, also verify the directory inside a running container.
The check runs `test -d' via `podman exec' or `docker exec', which needs
a running container and a shell utility in the image.  Anything inconclusive
counts as `unknown', so this can only ever forget more entries, never
fewer."
  :type 'boolean)

(defcustom project-liveness-remote-timeout 5
  "Seconds to wait for `file-exists-p' on a remote root before giving up.
A timeout yields `unknown'.  Best effort only: the timer can fire only
while Tramp yields, so lowering `tramp-connection-timeout' is the more
reliable knob."
  :type 'number)

(defcustom project-liveness-confirm t
  "When non-nil, list the doomed entries and confirm before forgetting."
  :type 'boolean)


;;; Container engines

;; Podman and Docker take the same subcommands for everything below except
;; the existence test, which is the one place their clients differ -- and
;; which client sits behind a Tramp method is a question for the client
;; itself, not for the method name: see `project-liveness--dialect'.

(defun project-liveness--program (engine)
  "Return the command-line client to invoke for ENGINE."
  (pcase-exhaustive engine
    ('podman (or project-liveness-podman-program
                 (and (boundp 'tramp-podman-program)
                      (symbol-value 'tramp-podman-program))
                 "podman"))
    ('docker (or project-liveness-docker-program
                 (and (boundp 'tramp-docker-program)
                      (symbol-value 'tramp-docker-program))
                 "docker"))))

(defun project-liveness--run (engine destination &rest args)
  "Run ENGINE's client with ARGS, sending output to DESTINATION.
Return the exit code, or 125 if the client could not be run at all."
  (condition-case nil
      (apply #'call-process (project-liveness--program engine)
             nil destination nil args)
    (error 125)))

(defvar project-liveness--dialect-cache (make-hash-table :test #'equal)
  "Memo from a client program to the dialect it turned out to speak.
Which binary is installed does not change under a running Emacs, unlike
everything else this library asks about, so the answer is kept for the
session; `clrhash' it after installing or replacing a client.")

(defun project-liveness--dialect (engine)
  "Return the dialect ENGINE's configured client actually speaks.
Debian's podman-docker package installs a /usr/bin/docker that execs
podman, and the two clients differ exactly where this library depends on
them: podman has `container exists', and reports its own errors as 125
where docker reports 1.  So ask the client what it is rather than
trusting the Tramp method: under the shim, \"docker --version\" answers
for podman.  A client that cannot be run at all keeps ENGINE, which
leaves every root under it `unknown'."
  (let* ((program (project-liveness--program engine))
         (cached (gethash program project-liveness--dialect-cache 'unset)))
    (if (not (eq cached 'unset))
        cached
      (puthash program
               (with-temp-buffer
                 ;; stderr lands here too, which is what we want: the shim
                 ;; announces itself there and nowhere else.
                 (if (and (eq 0 (project-liveness--run engine t "--version"))
                          (let ((case-fold-search t))
                            (string-match-p "podman" (buffer-string))))
                     'podman
                   engine))
               project-liveness--dialect-cache))))

(defun project-liveness--podman-container-status (engine name)
  "Return `exists', `missing', or `unknown' for NAME, asking ENGINE's client."
  (pcase (project-liveness--run engine nil "container" "exists" name)
    (0 'exists)
    (1 'missing)
    (_ 'unknown)))                      ; 125, or killed by a signal

(defun project-liveness--docker-daemon-p (engine)
  "Return non-nil if the Docker daemon behind ENGINE\='s client answers."
  (eq 0 (project-liveness--run
         engine nil "version" "--format" "{{.Server.Version}}")))

(defun project-liveness--docker-container-status (engine name)
  "Return `exists', `missing', or `unknown' for NAME, asking ENGINE's client.
Docker has no counterpart to \"podman container exists\", so \"container
inspect\" stands in for it."
  (pcase (project-liveness--run engine nil "container" "inspect" name)
    (0 'exists)
    ;; Exit 1 covers both \"no such container\" and a daemon that would not
    ;; answer at all, and only the daemon can tell the two apart.  Asking
    ;; it here rather than up front keeps the probe off the common path:
    ;; it costs a second call only for a container that already looks gone.
    (1 (if (project-liveness--docker-daemon-p engine) 'missing 'unknown))
    (_ 'unknown)))

(defun project-liveness-container-status (engine name)
  "Return `exists', `missing', or `unknown' for ENGINE's container NAME.
NAME may be a container name or an ID prefix.  A stopped container
still counts as `exists'."
  (pcase-exhaustive (project-liveness--dialect engine)
    ('podman (project-liveness--podman-container-status engine name))
    ('docker (project-liveness--docker-container-status engine name))))

(defun project-liveness--container-running-p (engine name)
  "Return non-nil if ENGINE's container NAME is currently running."
  (with-temp-buffer
    (and (eq 0 (project-liveness--run
                engine t "container" "inspect"
                "--format" "{{.State.Running}}" name))
         (string-prefix-p "true" (string-trim (buffer-string))))))

(defun project-liveness--container-path-status (engine name path)
  "Return `exists', `missing', or `unknown' for PATH in NAME under ENGINE."
  (if (not (project-liveness--container-running-p engine name))
      'unknown                          ; cannot look inside a stopped container
    (pcase (project-liveness--run engine nil "exec" name "test" "-d" path)
      (0 'exists)
      (1 'missing)
      (_ 'unknown))))                   ; no `test' in the image, exec refused

(defun project-liveness--container-status (root engine)
  "Return the status of container ROOT under ENGINE.
No Tramp connection is opened."
  (let ((name (file-remote-p root 'host))
        (path (file-remote-p root 'localname)))
    (pcase (project-liveness-container-status engine name)
      ;; Only an absolute localname can be checked inside the container:
      ;; Tramp writes container roots under $HOME as "~/src/foo", `exec'
      ;; runs `test' with no shell to expand that, and the answer would be
      ;; `missing' for a directory that is plainly there.  Expanding the
      ;; tilde first is not an option -- that means asking the host for its
      ;; home directory, which is a connection.
      ('exists (if (and project-liveness-check-container-path
                        path (string-prefix-p "/" path))
                   (project-liveness--container-path-status engine name path)
                 'exists))
      (status status))))


;;; Generic roots

(defun project-liveness--file-status (root)
  "Return the status of ROOT according to `file-exists-p'.
Errors and timeouts yield `unknown' rather than `missing'."
  (condition-case nil
      (with-timeout (project-liveness-remote-timeout 'unknown)
        (if (file-exists-p root) 'exists 'missing))
    (error 'unknown)))

(defun project-liveness-root-status (root)
  "Return `exists', `missing', or `unknown' for project ROOT."
  (let* ((method (file-remote-p root 'method))
         (engine (cdr (assoc method project-liveness-container-methods))))
    (cond
     ((null method) (project-liveness--file-status root))
     (engine (project-liveness--container-status root engine))
     ((eq project-liveness-remote-policy 'never) 'unknown)
     ((eq project-liveness-remote-policy 'always)
      (project-liveness--file-status root))
     ;; connected-only: judge it only if a connection is already open.
     ;; The third argument to `file-remote-p' is CONNECTED -- passing
     ;; `connected' as the second is a common and silent mistake.
     ((file-remote-p root nil t) (project-liveness--file-status root))
     (t 'unknown))))

(defun project-liveness-root-live-p (root)
  "Return non-nil unless project ROOT is positively known to be gone."
  (not (eq 'missing (project-liveness-root-status root))))

(defun project-liveness-zombie-p (root)
  "Return non-nil if project ROOT should be forgotten.
Suitable as a predicate in `project-prune-zombie-projects' (Emacs 31)."
  (eq 'missing (project-liveness-root-status root)))


;;; Forgetting

;; `project-forget-project' looks its argument up in the known-project list
;; under `abbreviate-file-name', which for a Tramp name is not the identity:
;; once Tramp has connected to a host it remembers that host's home
;; directory in its persistency file, so abbreviation rewrites
;; "/podman:c:/home/ht/src/" as "/podman:c:~/src/".  The list itself holds
;; remote roots verbatim -- `project--read-project-list' abbreviates only
;; local ones -- so the lookup misses, nothing is removed, and the caller is
;; still told the project was forgotten.  The entry survives, project.el goes
;; on offering it, and choosing it dials the container we just declared dead.
;;
;; Match the strings we were handed instead: they come from
;; `project-known-project-roots', so they are exactly the strings the list
;; holds.

(defun project-liveness--forget (roots)
  "Remove ROOTS from the known project list and save it.
Return the roots actually removed.  ROOTS are strings as returned by
`project-known-project-roots', matched literally."
  (project--ensure-read-project-list)
  (let ((doomed (seq-filter (lambda (entry) (member (car entry) roots))
                            project--list)))
    (when doomed
      (setq project--list (seq-difference project--list doomed))
      (project--write-project-list))
    (mapcar #'car doomed)))


;;; Commands

(defun project-liveness-dead-roots ()
  "Return the known project roots that are positively gone."
  (seq-remove #'project-liveness-root-live-p (project-known-project-roots)))

;;;###autoload
(defun project-liveness-list-dead-projects ()
  "Report which known projects would be forgotten, without forgetting any."
  (interactive)
  (let ((dead (project-liveness-dead-roots)))
    (if (null dead)
        (message "No dead projects")
      (message "%d dead project(s): %s" (length dead) (string-join dead ", ")))))

;;;###autoload
(defun project-liveness-forget-dead-projects (&optional dry-run)
  "Forget known projects whose roots are positively gone.
With a prefix argument, DRY-RUN, only report what would be removed.
Roots whose status cannot be established -- an unreachable host, a
podman that will not answer -- are always left alone."
  (interactive "P")
  (let ((dead (project-liveness-dead-roots)))
    (cond
     ((null dead) (message "No dead projects"))
     (dry-run (project-liveness-list-dead-projects))
     ((and project-liveness-confirm
           (called-interactively-p 'interactive)
           (not (y-or-n-p (format "Forget %d project(s): %s? "
                                  (length dead) (string-join dead ", ")))))
      (message "Nothing forgotten"))
     (t
      (let ((forgotten (project-liveness--forget dead)))
        (message "Forgot %d project(s): %s"
                 (length forgotten) (string-join forgotten ", ")))))))

(provide 'project-liveness)
;;; project-liveness.el ends here
