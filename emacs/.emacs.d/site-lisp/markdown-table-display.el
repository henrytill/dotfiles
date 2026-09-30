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

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)
(require 'seq)
(require 'subr-x)

(defvar markdown-table-display-mode)

(defvar-local markdown-table-display--table-at-point nil
  "Start of the table containing point, or nil.")

(defvar-local markdown-table-display--timer nil
  "Idle timer that redraws tables after a buffer change.")

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
        (unless (eq (char-before (1- (point))) ?\\)
          (push (1- (point)) pipes))))
    (setq pipes (nreverse pipes))
    ;; A row need not end with a pipe.
    (unless (string-blank-p (buffer-substring-no-properties (1+ (car (last pipes))) eol))
      (setq pipes (append pipes (list eol))))
    (cl-loop for (a b) on pipes while b
             collect (markdown-table-display--visible-string (1+ a) b))))

(defun markdown-table-display--alignment (cell)
  "Return the alignment given by delimiter row CELL: left, right or center."
  (let ((s (string-trim (substring-no-properties cell))))
    (cond ((and (string-prefix-p ":" s) (string-suffix-p ":" s) (> (length s) 1)) 'center)
          ((string-suffix-p ":" s) 'right)
          (t 'left))))

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
  "Pad string S to WIDTH according to ALIGNMENT."
  (let* ((gap (max 0 (- width (string-width s))))
         (left (pcase alignment
                 ('right gap)
                 ('center (/ gap 2))
                 (_ 0))))
    (concat (make-string left ?\s) s (make-string (- gap left) ?\s))))

(defun markdown-table-display--render (beg end)
  "Return the drawing of the table between BEG and END."
  (let (rows delimiter)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (if (and (null delimiter) (looking-at-p markdown-table-hline-regexp))
            (setq delimiter (markdown-table-display--cells))
          (push (markdown-table-display--cells) rows))
        (forward-line 1)))
    (setq rows (nreverse rows))
    (let* ((ncols (apply #'max (length delimiter) (mapcar #'length rows)))
           (rows (mapcar (lambda (r) (append r (make-list (- ncols (length r)) ""))) rows))
           (alignments (cl-loop for i below ncols
                                collect (markdown-table-display--alignment (or (nth i delimiter) ""))))
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
               do (when (or (= n 1) (and multiline (> n 1)))
                    ;; A rule under the header, and between rows once
                    ;; any cell has wrapped, so rows stay distinguishable.
                    (push rule lines))
                  (dotimes (k (apply #'max (mapcar #'length row)))
                    (push (concat bar
                                  (mapconcat (lambda (col)
                                               (concat " "
                                                       (markdown-table-display--pad
                                                        (or (nth k (nth col row)) "")
                                                        (nth col widths)
                                                        (nth col alignments))
                                                       " "))
                                             (number-sequence 0 (1- ncols))
                                             bar)
                                  bar)
                          lines)))
      (when (and delimiter (= (length rows) 1))
        (push rule lines))
      (string-join (nreverse lines) "\n"))))

(defun markdown-table-display--tables ()
  "Return a list of (BEG . END) for each table in the buffer."
  (let (tables)
    (save-excursion
      (syntax-propertize (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (if (not (markdown-table-at-point-p))
            (forward-line 1)
          (let ((beg (point)))
            (while (and (not (eobp)) (markdown-table-at-point-p))
              (forward-line 1))
            (push (cons beg (save-excursion (skip-chars-backward "\n") (point))) tables)))))
    (nreverse tables)))

(defun markdown-table-display--remove ()
  "Remove all table drawings from the buffer."
  (remove-overlays (point-min) (point-max) 'markdown-table-display t))

(defun markdown-table-display-refresh ()
  "Draw every table in the buffer except the one containing point."
  (interactive)
  (markdown-table-display--remove)
  (font-lock-ensure)
  (save-restriction
    (widen)
    (pcase-dolist (`(,beg . ,end) (markdown-table-display--tables))
      (unless (<= beg (point) end)
        (let ((ov (make-overlay beg end nil t nil)))
          (overlay-put ov 'markdown-table-display t)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'display (markdown-table-display--render beg end)))))))

(defun markdown-table-display--post-command ()
  "Redraw tables when point enters or leaves one."
  (let ((table (and (markdown-table-at-point-p) (markdown-table-begin))))
    (unless (eql table markdown-table-display--table-at-point)
      (setq markdown-table-display--table-at-point table)
      (markdown-table-display-refresh))))

(defun markdown-table-display--after-change (&rest _)
  "Schedule a redraw of the tables once Emacs is idle."
  (when (timerp markdown-table-display--timer)
    (cancel-timer markdown-table-display--timer))
  (setq markdown-table-display--timer
        (run-with-idle-timer 0.5 nil #'markdown-table-display--refresh-buffer
                             (current-buffer))))

(defun markdown-table-display--refresh-buffer (buffer)
  "Redraw the tables in BUFFER, if it is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when markdown-table-display-mode
        (markdown-table-display-refresh)))))

;;;###autoload
(define-minor-mode markdown-table-display-mode
  "Draw markdown tables as aligned grids that wrap to fit `fill-column'.
The buffer text is not changed.  The table under point shows its raw
text for editing."
  :lighter nil
  (markdown-table-display--remove)
  (remove-hook 'post-command-hook #'markdown-table-display--post-command t)
  (remove-hook 'after-change-functions #'markdown-table-display--after-change t)
  (when markdown-table-display-mode
    (add-hook 'post-command-hook #'markdown-table-display--post-command nil t)
    (add-hook 'after-change-functions #'markdown-table-display--after-change nil t)
    ;; Wait until the buffer is set up and fontified before drawing.
    (markdown-table-display--after-change)))

(provide 'markdown-table-display)
;;; markdown-table-display.el ends here
