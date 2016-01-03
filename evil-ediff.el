;;; evil-ediff.el --- Make ediff a little evil

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; Package-Requires: ((evil "1.2.3"))
;; Homepage: https://github.com/justbur/evil-ediff
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

;; Make ediff a little evil. This configures ediff to be a little more friendly
;; users of vim-like keybindings. Consult the help buffer (=?=) for more info.

;; Here's a table describing the bindings

;; | Command                     | Original Binding | Evil-ediff  |
;; |-----------------------------+------------------+-------------|
;; | ediff-jump-to-difference    | j                | d           |
;; | ediff-previous-difference   | p,DEL            | C-k,p,DEL   |
;; | ediff-next-difference       | n,SPC            | C-j,n,SPC   |
;; | jump to first difference    | 1j               | gg (or 1d)  |
;; | jump to last difference     | N/A              | G           |
;; | ediff-next-difference       | n,SPC            | C-j,n,SPC   |
;; | scroll down 1 line          | N/A              | j           |
;; | scroll up 1 line            | N/A              | k           |
;; | scroll down half page       | v,C-v            | C-d,v,C-v   |
;; | scroll up half page         | V,M-v            | C-u,V,M-v   |
;; | ediff-suspend               | z                | C-z         |
;; | scroll left                 | >                | zh          |
;; | scroll right                | <                | zl          |

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

(defun evil-ediff-first-difference ()
  (interactive)
  (ediff-jump-to-difference 1))

(defun evil-ediff-last-difference ()
  (interactive)
  (ediff-jump-to-difference ediff-number-of-differences))

(defvar evil-ediff-bindings
  '(("d"    . ediff-jump-to-difference)
    ("j"    . evil-ediff-scroll-down-1)
    ("k"    . evil-ediff-scroll-up-1)
    ("\C-j" . ediff-next-difference)
    ("\C-k" . ediff-previous-difference)
    ("gg"   . evil-ediff-first-difference)
    ("G"    . evil-ediff-last-difference)
    ("\C-d" . evil-ediff-scroll-down)
    ("\C-u" . evil-ediff-scroll-up)
    ("\C-z" . ediff-suspend)
    ("z"    . nil)
    ("zl"   . evil-ediff-scroll-right)
    ("zh"   . evil-ediff-scroll-left)))

(evil-set-initial-state 'ediff-mode 'normal)
(defun evil-ediff-startup-hook ()
  "Place evil-ediff bindings in `ediff-mode-map'."
  (evil-make-overriding-map ediff-mode-map 'normal)
  (dolist (entry evil-ediff-bindings)
    (define-key ediff-mode-map (car entry) (cdr entry)))
  (evil-normalize-keymaps)
  nil)

(defun evil-ediff-init ()
  "Initialize evil-ediff."
  (interactive)
  (add-hook 'ediff-startup-hook 'evil-ediff-startup-hook)
  (evil-ediff-adjust-help))
(evil-ediff-init)

(defun evil-ediff-revert ()
  "Revert changes made by evil-ediff."
  (interactive)
  (unless evil-ediff-help-changed
    (dolist (msg
             '((ediff-long-help-message-compare2 . ediff-long-help-message-compare2-backup)
               (ediff-long-help-message-compare3 . ediff-long-help-message-compare3-backup)
               (ediff-long-help-message-narrow2 . ediff-long-help-message-narrow2-backup)
               (ediff-long-help-message-word-mode . ediff-long-help-message-word-mode-backup)
               (ediff-long-help-message-merge . ediff-long-help-message-merge-backup)
               (ediff-long-help-message-head . ediff-long-help-message-head-backup)
               (ediff-long-help-message-tail . ediff-long-help-message-tail-backup)))
      (setf (symbol-value (car msg)) (symbol-value (cdr msg)))))
  (setq evil-ediff-help-changed nil)
  (add-hook 'ediff-startup-hook 'evil-ediff-startup-hook))

(provide 'evil-ediff)
;;; evil-ediff.el ends here
