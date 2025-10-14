;;; niminal-mode.el --- A small major mode for editing Nim code.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Henry Till

;; Author: Henry Till <ht@thalassa>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
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

;; 

;;; Code:

(define-derived-mode niminal-mode prog-mode "Nim"
  "A minimal major mode for editing Nim code."
  (setq-local indent-tabs-mode nil
              tab-width 2
              indent-line-function #'tab-to-tab-stop))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nim\\'" . niminal-mode))

(provide 'niminal-mode)
;;; niminal-mode.el ends here
