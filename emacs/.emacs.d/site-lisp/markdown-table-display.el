;;; markdown-table-display.el --- Display markdown tables aligned and wrapped  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Henry Till

;; Author: Henry Till <henrytill@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.5"))
;; Keywords: markdown, convenience

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

;; `markdown-table-display-mode' draws each pipe table as an aligned
;; grid that fits within `fill-column' (or `visual-fill-column-width'),
;; word-wrapping cell text onto extra lines when the table would
;; otherwise be too wide.  The drawing is an overlay `display' string,
;; so the buffer text is never changed.
;;
;; The table under point is shown as its raw text, so it can be
;; edited; it is drawn again once point leaves it.  Cell widths are
;; measured on the text as displayed, so markup hidden by
;; `markdown-hide-markup' doesn't count.
;;
;; Tables are drawn from `jit-lock-functions', after font-lock, so
;; only the tables redisplay reaches are drawn, and anything that has
;; jit-lock refontify a table, such as `font-lock-flush', redraws it.

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)
(require 'seq)
(require 'subr-x)

(defvar-local markdown-table-display--raw nil
  "Markers (BEG . END) around the table shown as raw text, or nil.")

(defvar-local markdown-table-display--drawn-width nil
  "The `markdown-table-display--width' tables were last drawn at.")

(defun markdown-table-display--width ()
  "Return the maximum width of a drawn table."
  ;; One column less, since a line that exactly fills the text area
  ;; still wraps on a terminal.
  (1- (or (bound-and-true-p visual-fill-column-width) fill-column)))

(defun markdown-table-display--visible-string (beg end)
  "Return the text from BEG to END that is not invisible, with its faces."
  (let ((pos beg)
        (parts nil))
    (while (< pos end)
      (let ((next (next-single-char-property-change pos 'invisible nil end)))
        (unless (invisible-p pos)
          (push (buffer-substring pos next) parts))
        (setq pos next)))
    (let ((s (string-trim (apply #'concat (nreverse parts)))))
      (remove-text-properties 0 (length s)
                              '(display nil composition nil wrap-prefix nil
                                invisible nil keymap nil mouse-face nil)
                              s)
      s)))

(defun markdown-table-display--cells ()
  "Return the cells of the table row at point as a list of strings."
  (let ((bol (pos-bol))
        (eol (pos-eol))
        (pipes nil))
    (save-excursion
      (goto-char bol)
      (while (re-search-forward "|" eol t)
        ;; Skip the pipes that `markdown-table-align' doesn't split on.
        (let ((pipe (1- (point))))
          (unless (or (eq (char-before pipe) ?\\)
                      (markdown--face-p pipe '(markdown-inline-code-face))
                      (markdown--thing-at-wiki-link pipe))
            (push pipe pipes)))))
    (setq pipes (nreverse pipes))
    ;; A row need not end with a pipe.
    (unless (string-blank-p (buffer-substring-no-properties (1+ (car (last pipes))) eol))
      (setq pipes (append pipes (list eol))))
    (cl-loop for (a b) on pipes while b
             collect (markdown-table-display--visible-string (1+ a) b))))

(defun markdown-table-display--column-widths (naturals available)
  "Share AVAILABLE columns among columns with NATURALS widths.
Columns that need less than an even share keep their natural width,
and what they leave is shared among the rest."
  (let* ((order (sort (number-sequence 0 (1- (length naturals)))
                      (lambda (a b) (< (nth a naturals) (nth b naturals)))))
         (widths (make-vector (length naturals) 0))
         (remaining available)
         (left (length naturals)))
    (dolist (i order)
      (let ((w (max 3 (min (nth i naturals) (/ remaining left)))))
        (aset widths i w)
        (setq remaining (- remaining w)
              left (1- left))))
    (append widths nil)))

(defun markdown-table-display--wrap (s width)
  "Word-wrap string S into a list of lines at most WIDTH wide."
  (let ((lines nil)
        (line nil))
    (dolist (word (split-string s "[ \t]+" t))
      ;; Break words that are too long to fit on a line of their own.
      (while (> (string-width word) width)
        (when line (push line lines) (setq line nil))
        (let ((head (truncate-string-to-width word width)))
          (push head lines)
          (setq word (substring word (length head)))))
      (cond ((null line) (setq line word))
            ((<= (+ (string-width line) 1 (string-width word)) width)
             (setq line (concat line " " word)))
            (t (push line lines) (setq line word))))
    (when line (push line lines))
    (or (nreverse lines) (list ""))))

(defun markdown-table-display--pad (s width alignment)
  "Pad string S to WIDTH according to ALIGNMENT.
ALIGNMENT is a column format from `markdown-table-colfmt'."
  (let* ((gap (max 0 (- width (string-width s))))
         (left (pcase alignment
                 ('r gap)
                 ('c (/ gap 2))
                 (_ 0))))
    (concat (make-string left ?\s) s (make-string (- gap left) ?\s))))

(defun markdown-table-display--render (beg end)
  "Return the drawing of the table between BEG and END."
  (let (rows delimiter)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (if (and (null delimiter) (looking-at-p markdown-table-hline-regexp))
            (setq delimiter (markdown-table-colfmt
                             (buffer-substring-no-properties (pos-bol) (pos-eol))))
          (push (markdown-table-display--cells) rows))
        (forward-line 1)))
    (setq rows (nreverse rows))
    (let* ((ncols (apply #'max (length delimiter) (mapcar #'length rows)))
           (rows (mapcar (lambda (r) (append r (make-list (- ncols (length r)) ""))) rows))
           (alignments (cl-loop for i below ncols collect (nth i delimiter)))
           (naturals (cl-loop for i below ncols
                              collect (apply #'max 1 (mapcar (lambda (r) (string-width (nth i r))) rows))))
           ;; Each column costs its width plus " | ".
           (widths (markdown-table-display--column-widths
                    naturals (- (markdown-table-display--width) 1 (* 3 ncols))))
           (wrapped (mapcar (lambda (r) (cl-mapcar #'markdown-table-display--wrap r widths)) rows))
           (multiline (seq-some (lambda (r) (seq-some #'cdr r)) wrapped))
           (bar (propertize "|" 'face 'markdown-table-face))
           (rule (propertize (concat "|" (mapconcat (lambda (w) (make-string (+ w 2) ?-)) widths "|") "|")
                             'face 'markdown-table-face))
           (lines nil))
      (cl-loop for row in wrapped
               for n from 0
               do (when (if multiline (> n 0) (= n 1))
                    ;; A rule under the header, and between rows once
                    ;; any cell has wrapped, so rows stay distinguishable.
                    (push rule lines))
                  (dotimes (k (apply #'max (mapcar #'length row)))
                    (push (concat bar
                                  (mapconcat #'identity
                                             (cl-mapcar (lambda (cell width alignment)
                                                          (concat " "
                                                                  (markdown-table-display--pad
                                                                   (or (nth k cell) "")
                                                                   width alignment)
                                                                  " "))
                                                        row widths alignments)
                                             bar)
                                  bar)
                          lines)))
      (when (and delimiter (= (length rows) 1))
        (push rule lines))
      (string-join (nreverse lines) "\n"))))

(defun markdown-table-display--table-end ()
  "Return the end of the last row of the table at point."
  (save-excursion
    (goto-char (markdown-table-end))
    (skip-chars-backward "\n")
    (point)))

(defun markdown-table-display--tables (beg end)
  "Return a list of (BEG . END) for each table overlapping BEG to END."
  (let (tables)
    (save-excursion
      ;; Code blocks are found by syntax properties.  A table ends at
      ;; the next blank line, so there is no need to look further.
      (goto-char end)
      (syntax-propertize (if (re-search-forward "^[ \t]*$" nil t) (point) (point-max)))
      (goto-char beg)
      ;; A table doesn't include the newline ending its last row.
      (forward-line (if (eolp) 1 0))
      (when (markdown-table-at-point-p)
        (goto-char (markdown-table-begin)))
      (while (and (< (point) end)
                  (re-search-forward markdown-table-line-regexp end t))
        (forward-line 0)
        (if (not (markdown-table-at-point-p))
            (forward-line 1)
          (push (cons (point) (markdown-table-display--table-end)) tables)
          (goto-char (markdown-table-end)))))
    (nreverse tables)))

(defun markdown-table-display--drawings (beg end)
  "Return the overlays drawing tables that overlap BEG to END."
  (seq-filter (lambda (ov) (overlay-get ov 'markdown-table-display))
              (overlays-in beg end)))

(defun markdown-table-display--undraw (beg end)
  "Remove the drawings of tables overlapping BEG to END."
  (mapc #'delete-overlay (markdown-table-display--drawings beg end)))

(defun markdown-table-display--raw-p (beg end)
  "Return non-nil if the table from BEG to END is shown as raw text.
It may have grown past the raw text's markers, by joining another table."
  ;; Not point, which other `jit-lock-functions' may have moved.
  (let ((raw markdown-table-display--raw))
    (and raw (<= beg (cdr raw)) (<= (car raw) end))))

(defvar font-lock-beg)
(defvar font-lock-end)

(defun markdown-table-display--extend-font-lock-region ()
  "Extend the region font-lock fontifies over whole tables to be drawn.
They are drawn after font-lock, from text it must have fontified.
For `font-lock-extend-region-functions'."
  (let ((beg font-lock-beg)
        (end font-lock-end))
    (pcase-dolist (`(,tbeg . ,tend) (markdown-table-display--tables beg end))
      (unless (markdown-table-display--raw-p tbeg tend)
        (setq font-lock-beg (min font-lock-beg tbeg)
              font-lock-end (max font-lock-end tend))))
    (not (and (= beg font-lock-beg) (= end font-lock-end)))))

(defun markdown-table-display--fontify (beg end)
  "Redraw the tables overlapping BEG to END, except the raw one.
Return BEG to END extended over those tables, as `jit-lock-functions'
may."
  (save-restriction
    (widen)
    (let ((tables (markdown-table-display--tables beg end)))
      (when tables
        (setq beg (min beg (caar tables))
              end (max end (cdar (last tables)))))
      (markdown-table-display--undraw beg end)
      (pcase-dolist (`(,tbeg . ,tend) tables)
        (unless (markdown-table-display--raw-p tbeg tend)
          (let ((ov (make-overlay tbeg tend nil t nil)))
            (overlay-put ov 'markdown-table-display t)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'display (markdown-table-display--render tbeg tend))))))
    `(jit-lock-bounds ,beg . ,end)))

(defun markdown-table-display-refresh ()
  "Have every table in the buffer redrawn except the raw one."
  (interactive)
  (jit-lock-refontify))

(defun markdown-table-display--post-command ()
  "Show the table at point as raw text, and have the one point left redrawn.
Have every table redrawn if the width to draw them at has changed."
  (let ((width (markdown-table-display--width)))
    (unless (eql width markdown-table-display--drawn-width)
      (setq markdown-table-display--drawn-width width)
      (jit-lock-refontify)))
  (let ((raw markdown-table-display--raw))
    (unless (and raw (<= (car raw) (point) (cdr raw)))
      (when raw
        (setq markdown-table-display--raw nil)
        (jit-lock-refontify (car raw) (cdr raw))
        (set-marker (car raw) nil)
        (set-marker (cdr raw) nil))
      (when (markdown-table-at-point-p)
        (let ((beg (markdown-table-begin))
              (end (markdown-table-display--table-end)))
          (setq markdown-table-display--raw
                (cons (copy-marker beg) (copy-marker end t)))
          (markdown-table-display--undraw beg end))))))

(defvar jit-lock-start)
(defvar jit-lock-end)

(defun markdown-table-display--extend-after-change (beg end _len)
  "Have jit-lock refontify all of each drawn table from BEG to END.
Redisplay never looks at the text under a drawing, so it wouldn't
notice that jit-lock had marked only the changed lines.
For `jit-lock-after-change-extend-region-functions'."
  (dolist (ov (markdown-table-display--drawings beg end))
    (setq jit-lock-start (min jit-lock-start (overlay-start ov))
          jit-lock-end (max jit-lock-end (overlay-end ov)))))

;;;###autoload
(define-minor-mode markdown-table-display-mode
  "Draw markdown tables as aligned grids that wrap to fit `fill-column'.
The buffer text is not changed.  The table under point shows its raw
text for editing."
  :lighter nil
  (jit-lock-unregister #'markdown-table-display--fontify)
  (save-restriction
    (widen)
    (markdown-table-display--undraw (point-min) (point-max)))
  (setq markdown-table-display--raw nil)
  (remove-hook 'post-command-hook #'markdown-table-display--post-command t)
  (remove-hook 'font-lock-extend-region-functions
               #'markdown-table-display--extend-font-lock-region t)
  (remove-hook 'jit-lock-after-change-extend-region-functions
               #'markdown-table-display--extend-after-change t)
  (when markdown-table-display-mode
    (add-hook 'post-command-hook #'markdown-table-display--post-command nil t)
    (add-hook 'font-lock-extend-region-functions
              #'markdown-table-display--extend-font-lock-region nil t)
    (add-hook 'jit-lock-after-change-extend-region-functions
              #'markdown-table-display--extend-after-change nil t)
    ;; Run after font-lock, so drawings carry its faces, but
    ;; `jit-lock-register' can't append (bug#15155).
    (add-hook 'jit-lock-functions #'markdown-table-display--fontify 'append t)
    (jit-lock-register #'markdown-table-display--fontify)
    (setq markdown-table-display--drawn-width (markdown-table-display--width))
    (markdown-table-display--post-command)
    (jit-lock-refontify)))

(provide 'markdown-table-display)
;;; markdown-table-display.el ends here
