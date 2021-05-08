;;; md-outline-list.el --- outline list items in markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  DREWOR020

;; Author: Tobias Zawada <i@tn-home.de>
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

;; Markdown-mode uses `outline-minor-mode' for folding headlines.
;; This package extends it to item lists.

;;; Installation

;; Just put this package in your `load-path' and load it.
;;; Code:

(require 'outline)
(require 'markdown-mode)

(defcustom md-outline-list-regexp "\\([[:blank:]]*\\)-[[:blank:]]" ;;
  "Regular expression for outlining lists in `markdown-mode'."
  :group 'md-outline-list
  :type 'string)

(defconst md-outline-list-subexp 7
  "Subexpression in `outline-regexp' matching list markers.")

(defcustom md-outline-list-start-level 7
  "First outline level for lists."
  :group 'md-outline-list
  :type 'wholenump)

(defun md-outline-list-level (fun)
  "Consider also lists for computing the outline level in `markdown-mode'.
Sorry folks, this must be an advice for `markdown-outline-level' as FUN.
Setting variable `outline-level' to `md-outline-list-level' does not work
since `markdown-outline-level' is also called directly."
  (let ((list-level-str (and (match-beginning md-outline-list-subexp) (match-string md-outline-list-subexp))))
    (or (and list-level-str
	     (+ (length list-level-str) md-outline-list-start-level))
	(funcall fun))))

(advice-add 'markdown-outline-level :around #'md-outline-list-level)

(defconst md-outline-list-regexp
  (mapconcat #'identity
	     (list
	      markdown-regex-header
	      markdown-regex-list)
	     "\\|")
  "Used for `outline-regexp' in `markdown-mode' when `md-outline-list-mode' is active.")

(defun md-outline-list-toggle-children (event)
  "Goto window and point of EVENT and `outline-toggle-children'."
  (interactive "e")
  (unless (mouse-event-p event)
    (user-error "Argument of `md-toggle-children' should be a mouse event"))
  (let ((posn (event-start event)))
    (with-selected-window (posn-window posn)
      (save-excursion
	(goto-char (posn-point posn))
	(outline-toggle-children)))))

(defvar md-outline-list-mouse-keymap nil
  "Mouse keymap used at list bullets.")
(setq md-outline-list-mouse-keymap
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<down-mouse-1>")
	  #'md-outline-list-toggle-children)
	map))

(defface md-outline-list-mouse-face '((t :inherit highlight))
  "Face for highlighting bullets with mouse overing on them.
This indicates that you can toggle the folding by mouse."
  :group 'md-outline-list)

(defun md-outline-list-search-heading (bound)
  "Search for Markdown headings up to BOUND.
Set match data for the heading marker."
  (when (markdown-match-inline-generic (concat "\\(" markdown-regex-header "\\)") bound)
    (set-match-data
     (cl-loop for i from 3 upto 5
	      if (match-beginning i)
	      return (list (match-beginning i)
			   (match-end i)
			   (current-buffer))))
    (point)))

(defvar md-outline-list-keywords
  `((markdown-match-list-items
     2
     '(face
       default
       mouse-face md-outline-list-mouse-face
       keymap
       ,md-outline-list-mouse-keymap)
     append
     t)
    (md-outline-list-search-heading
     0
     '(face
       default
       mouse-face md-outline-list-mouse-face
       keymap
       ,md-outline-list-mouse-keymap)
     append
     t))
  "Font lock keywords for folding list items with mouse clicks.")

(define-minor-mode md-outline-list-mode
  "Outline lists in `markdown-mode'."
  nil nil nil
  (if md-outline-list-mode
      (progn
	(setq outline-regexp md-outline-list-regexp)
	(outline-minor-mode)
	(font-lock-add-keywords nil md-outline-list-keywords t))
    (setq outline-regexp markdown-regex-header)
    (outline-minor-mode -1)
    (font-lock-remove-keywords nil md-outline-list-keywords)))

(add-hook 'markdown-mode-hook #'md-outline-list-mode)

(provide 'md-outline-list)
;;; md-outline-list.el ends here
