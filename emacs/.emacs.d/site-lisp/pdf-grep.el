;;; pdf-grep.el --- Grep PDF files with pdfgrep, in parallel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Henry Till

;; Author: Henry Till <henrytill@gmail.com>
;; Keywords: files, matching

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

;; `pdf-grep' searches PDF files under a directory using the external
;; pdfgrep(1) program, driving GNU parallel(1) so that several files are
;; searched at once.  It is a thin wrapper around `rgrep': it binds
;; `grep-find-template' and lets grep.el do the quoting, the pruning of
;; `grep-find-ignored-directories', and the smart-case flag, so results
;; land in the usual *grep* buffer and \\[next-error] works as normal.
;;
;; parallel(1) buffers each job's output and emits it as a unit, so lines
;; from concurrent pdfgrep processes cannot interleave the way they can
;; with `xargs -P'.
;;
;; pdfgrep is passed --cache, so the text it extracts from each file is
;; reused by later searches.  The cache lives under ~/.cache/pdfgrep and is
;; keyed by file content, so it does not go stale when a file is edited or
;; moved; it can simply be deleted to reclaim the space.
;;
;; find(1) is invoked with -L, which dereferences symlinks, so that files
;; in a git-annex repository -- which are symlinks into
;; .git/annex/objects -- are searched.  Symlinks whose content is not
;; present locally remain -type l and are skipped rather than reported as
;; errors.

;;; Code:

(require 'grep)

(declare-function doc-view-goto-page "doc-view" (page))

(defgroup pdf-grep nil
  "Grep PDF files with pdfgrep and GNU parallel."
  :group 'grep)

(defcustom pdf-grep-program "pdfgrep"
  "The pdfgrep program used by `pdf-grep'."
  :type 'string)

(defcustom pdf-grep-parallel-program "parallel"
  "The GNU parallel program used by `pdf-grep'."
  :type 'string)

(defcustom pdf-grep-jobs nil
  "Value passed to parallel's --jobs option, or nil for its default.
The default runs one job per available core.  May be a number, or a
string such as \"200%\" -- see the parallel(1) manual."
  :type '(choice (const :tag "One job per core" nil)
                 natnum
                 string))

(defun pdf-grep--template ()
  "Return a `grep-find-template' running pdfgrep under GNU parallel.
The job template ends in \"; true\" because parallel exits with the
number of failed jobs, and pdfgrep exits 1 for each file it finds no
match in -- which otherwise reports a successful search as \"Grep exited
abnormally with code 101\" (parallel's code for more than 100 failed
jobs).  Normalizing each job's status leaves parallel's own failures,
such as 255, visible.  Runs that find nothing are still reported as
such: `grep-exit-message' distinguishes them by whether any output was
produced, not by the exit code."
  (format "find -L <D> <X> -type f <F> -print0 | %s --null%s %s --cache <C> -Hn -e <R> {} \\; true"
          pdf-grep-parallel-program
          (if pdf-grep-jobs (format " --jobs %s" pdf-grep-jobs) "")
          pdf-grep-program))

;;;###autoload
(defun pdf-grep (regexp directory)
  "Recursively grep for REGEXP in PDF files under DIRECTORY using pdfgrep.
Files are searched concurrently by GNU parallel; see `pdf-grep-jobs'.

REGEXP is matched case-insensitively unless it contains upper case
characters, as with \\[rgrep]."
  (interactive
   (list (read-string "PDF Grep regexp: ")
         (read-directory-name "Directory: ")))
  (dolist (program (list pdf-grep-program pdf-grep-parallel-program))
    (unless (executable-find program)
      (user-error "Cannot find %s" program)))
  ;; Compute the defaults before binding the template: `rgrep' calls
  ;; `grep-compute-defaults' when `grep-command' is unset, which would
  ;; assign to `grep-find-template' inside the binding below.
  (grep-compute-defaults)
  (let ((grep-find-template (pdf-grep--template))
        (grep-find-ignored-files nil))
    (rgrep regexp "*.pdf" (expand-file-name directory))))


;;; Visiting a hit at the right page

;; pdfgrep reports page numbers where grep reports line numbers, so the
;; motion `compilation-goto-locus' performs -- going to line N -- lands in
;; the raw PDF data rather than on the page that matched.  Correct it
;; afterwards.  `next-error-move-function' looks like the intended hook for
;; this, but `compilation-goto-locus' consults it only when narrowing
;; prevented the jump, so it never runs on the normal path.

(defcustom pdf-grep-goto-page t
  "Whether visiting a `pdf-grep' hit should turn to the matching page.
Applies when the PDF is visited in `doc-view-mode'."
  :type 'boolean)

(defun pdf-grep--locus-page (msg)
  "Return the page number recorded in the compilation message at marker MSG."
  (let ((message (get-text-property (marker-position msg) 'compilation-message
                                    (marker-buffer msg))))
    (and message
         (compilation--loc->line (compilation--message->loc message)))))

(defun pdf-grep--goto-doc-view-page (msg mk &optional _end-mk)
  "Turn to the page recorded in MSG when MK's buffer displays a PDF."
  (let ((buffer (marker-buffer mk)))
    (when (and pdf-grep-goto-page
               (buffer-live-p buffer)
               (buffer-live-p (marker-buffer msg))
               (with-current-buffer buffer (derived-mode-p 'doc-view-mode)))
      (let ((page (pdf-grep--locus-page msg))
            (window (get-buffer-window buffer t)))
        (when (natnump page)
          (if window
              (with-selected-window window
                (doc-view-goto-page page))
            (with-current-buffer buffer
              (doc-view-goto-page page))))))))

(advice-add 'compilation-goto-locus :after #'pdf-grep--goto-doc-view-page)

(provide 'pdf-grep)
;;; pdf-grep.el ends here
