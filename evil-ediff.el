;;; evil-ediff.el --- evil-based key bindings for ediff

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; Package-Requires: ((evil "1.2.3"))
;; Version: 0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configures simple evil-like bindings for ediff

;;; Code:

(require 'evil)
(require 'ediff)

(defvar evil-ediff-long-help-message-compare2-backup ediff-long-help-message-compare2)
(defvar evil-ediff-long-help-message-compare3-backup  ediff-long-help-message-compare3)
(defvar evil-ediff-long-help-message-narrow2-backup  ediff-long-help-message-narrow2)
(defvar evil-ediff-long-help-message-word-backup  ediff-long-help-message-word-mode)
(defvar evil-ediff-long-help-message-merge-backup  ediff-long-help-message-merge)
(defvar evil-ediff-long-help-message-head-backup  ediff-long-help-message-head)
(defvar evil-ediff-long-help-message-tail-backup  ediff-long-help-message-tail)

(defvar evil-ediff-help-changed nil)

(defun evil-ediff-adjust-help ()
  (unless evil-ediff-help-changed
    (dolist (msg '(ediff-long-help-message-compare2
                   ediff-long-help-message-compare3
                   ediff-long-help-message-narrow2
                   ediff-long-help-message-word-mode
                   ediff-long-help-message-merge
                   ediff-long-help-message-head
                   ediff-long-help-message-tail))
      (dolist (chng '( ;;("^" . "  ")
                      ("p,DEL -previous diff " . "C-k,p -previous diff ")
                      ("n,SPC -next diff     " . "C-j,n -next diff     ")
                      ("    j -jump to diff  " . "    d -jump to diff  ")
                      ("  </> -scroll lt/rt  " . "zh/zl -scroll lt/rt  ")
                      ("  v/V -scroll up/dn  " . "C-u/d -scroll up/dn  ")
                      ("  z/q -suspend/quit  " . "C-z/q -quit/suspend  ")))
        (setf (symbol-value msg)
              (replace-regexp-in-string (car chng) (cdr chng) (symbol-value msg))))))
  (setq evil-ediff-help-changed t))
(evil-ediff-adjust-help)

(defun evil-ediff-scroll-down (&optional arg)
  (interactive "P")
  (let ((last-command-event ?v))
    (ediff-scroll-vertically arg)))

(defun evil-ediff-scroll-left (&optional arg)
  (interactive "P")
  (let ((last-command-event ?>))
    (ediff-scroll-horizontally arg)))

(defun evil-ediff-scroll-right (&optional arg)
  (interactive "P")
  (let ((last-command-event ?<))
    (ediff-scroll-horizontally arg)))

(defun evil-ediff-scroll-up (&optional arg)
  (interactive "P")
  (let ((last-command-event ?V))
    (ediff-scroll-vertically arg)))

(defun evil-ediff-scroll-down (&optional arg)
  (interactive "P")
  (let ((last-command-event ?v))
    (ediff-scroll-vertically arg)))

(defun evil-ediff-scroll-down-1 ()
  (interactive)
  (let ((last-command-event ?v))
    (ediff-scroll-vertically 1)))

(defun evil-ediff-scroll-up-1 ()
  (interactive)
  (let ((last-command-event ?V))
    (ediff-scroll-vertically 1)))

(setq evil-ediff-bindings
  '(("d"    . ediff-jump-to-difference)
    ("j"    . evil-ediff-scroll-down-1)
    ("k"    . evil-ediff-scroll-up-1)
    ("\C-j" . ediff-next-difference)
    ("\C-k" . ediff-previous-difference)
    ("\C-d" . evil-ediff-scroll-down)
    ("\C-u" . evil-ediff-scroll-up)
    ("\C-z" . ediff-suspend)
    ("z"    . nil)
    ("zl"   . evil-ediff-scroll-right)
    ("zh"   . evil-ediff-scroll-left)))

(evil-set-initial-state 'ediff-mode 'normal)
(defun evil-ediff-startup-hook ()
  (evil-make-overriding-map ediff-mode-map 'normal)
  (dolist (entry evil-ediff-bindings)
    (define-key ediff-mode-map (car entry) (cdr entry)))
  (evil-normalize-keymaps)
  nil)
(add-hook 'ediff-startup-hook 'evil-ediff-startup-hook)

(provide 'evil-ediff)
