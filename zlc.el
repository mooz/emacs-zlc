;;; zlc.el --- provides zsh like completion in the minibuffer

;; Copyright (C) 2010  mooz

;; Author:  mooz <stillpedant@gmail.com>
;; Keywords: matching, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Usage:

;; In your emacs config,
;; (require 'zlc)

;;; Code:

(defvar zlc--global-cache nil)
(defvar zlc--index 0)
(defvar zlc--field-begin 0)

;; Save completions
(defadvice display-completion-list (after zlc--save-global-cache activate)
  (setq zlc--global-cache (ad-get-arg 0)))

;; ============================================================ ;;
;; Private
;; ============================================================ ;;

(defun zlc--reset ()
  (setq zlc--field-begin (field-end)
        zlc--index 0))

;; ============================================================ ;;
;; Public
;; ============================================================ ;;

(defun zlc-select-next (&optional direction)
  (interactive)
  ;; clear previous completion
  (delete-region zlc--field-begin (field-end))
  (when (>= zlc--index 0)
    ;; select next completion
    (let* ((str (nth zlc--index zlc--global-cache))
           ;; sometimes (get-text-property 0 'face str) does not work...
           (from (if (eq (cadr (text-properties-at 0 str))
                         'completions-first-difference)
                     0
                   (or (next-property-change 0 str) 0))))
      (insert (substring str from))))
  (incf zlc--index (or direction 1))
  (setq zlc--index
        (cond
         ((>= zlc--index (length zlc--global-cache))
          -1)
         ((< zlc--index 0)
          (1- (length zlc--global-cache)))
         (t zlc--index))))

(defun zlc-select-previous ()
  (interactive)
  (zlc-select-next -1))

;; ============================================================ ;;
;; Overrides
;; ============================================================ ;;

(defun minibuffer-complete ()
  "Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
scroll the window of possible completions."
  (interactive)
  ;; reset when ...
  (unless (or (eq last-command this-command)
              (eq last-command 'zlc-select-previous))
    (setq minibuffer-scroll-window nil))
  (let ((window minibuffer-scroll-window))
    ;; If there's completions, select one of them orderly.
    (if (window-live-p window)
        (zlc-select-next 1)
      ;; otherwise, reset completions and arrange new one
      (zlc--reset)
      (case (completion--do-completion)
        (#b000 nil)
        (#b001 (goto-char (field-end))
               (minibuffer-message "Sole completion")
               t)
        (#b011 (goto-char (field-end))
               (minibuffer-message "Complete, but not unique")
               t)
        (t     t)))))

;; ============================================================ ;;
;; Settings
;; ============================================================ ;;

(let ((map minibuffer-local-map))
  (define-key map (kbd "<backtab>") 'zlc-select-previous))

(provide 'zlc)
;;; zlc.el ends here
