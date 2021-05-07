;;; md-outline-list.el --- outline list items in markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  DREWOR020

;; Author: DREWOR020 <toz@smtp.1und1.de>
;; Keywords: outlines

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

(defcustom md-outline-list-regexp "\\([[:blank:]]*\\)-[[:blank:]]" ;;
  "Regular expression for outlining lists in `markdown-mode'."
  :group 'md-outline-list
  :type 'string)
(defconst md-outline-list-subexp 7
  "Subexpression in outline-regexp matching list markers.")
(defcustom md-outline-list-start-level 7
  "First outline level for lists."
  :group 'md-outline-list
  :type 'wholenump)
(defun md-outline-list-level (fun)
  "Consider also lists for computing the outline level in `markdown-mode'.
Sorry folks, this must be an advice for `markdown-outline-level'.
Setting `outline-level' to `md-outline-list-level' does not work
since `markdown-outline-level' is also called directly."
  (let ((list-level-str (and (match-beginning md-outline-list-subexp) (match-string md-outline-list-subexp))))
    (or (and list-level-str
	     (+ (length list-level-str) md-outline-list-start-level))
	(funcall fun))))

(defconst md-outline-list-regexp
  (mapconcat #'identity
	     (list
	      markdown-regex-header
	      markdown-regex-list)
	     "\\|")
  "")

(defvar md-outline-list-mouse-keymap nil
  "Mouse keymap used at list bullets.")
(setq md-outline-list-mouse-keymap
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<down-mouse-1>")
	  #'outline-hide-subtree)))

(defvar md-outline-list-keywords
  `((markdown-match-list-items
     2
     (face
      default
      mouse-face highlight
      keymap
      ,md-outline-list-mouse-keymap)
     append
     t))
  "")

(defun md-outline-list ()
  "Outline lists in `markdown-mode'."
  (setq outline-regexp md-outline-list-regexp)
  (advice-add 'markdown-outline-level :around #'md-outline-list-level)
  (outline-minor-mode)
  (font-lock-add-keywords nil md-outline-list-keywords t))
(add-hook 'markdown-mode-hook #'md-outline-list)

(provide 'md-outline-list)
;;; md-outline-list.el ends here
