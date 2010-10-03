;;; zlc.el --- Provides zsh like completion for minibuffer

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
(defvar zlc--index -1)
(defvar zlc--field-begin 0)
(defvar zlc--previous-overlay nil)

(defface zlc-selected-completion-face
  '((t
     (:foreground "white"
      :background "firebrick"
      :italic nil
      :bold t)))
  "Style of selected item in *Completions* buffer")

(defcustom zlc-select-completion-immediately nil
  "Non-nil to select completion immediately when completion list created."
  :type 'boolean
  :group 'zlc)

;; Save completions
(defadvice display-completion-list (after zlc--save-global-cache activate)
  (setq zlc--global-cache (ad-get-arg 0)))

;; ============================================================ ;;
;; Private
;; ============================================================ ;;

(defsubst zlc--current-candidate ()
  (nth zlc--index zlc--global-cache))

(defsubst zlc--reset ()
  (setq zlc--field-begin (field-end)
        zlc--index -1))

(defsubst zlc--clear-overlay ()
  (when zlc--previous-overlay
    (delete-overlay zlc--previous-overlay)))

(defsubst zlc--ensure-visible (win p)
  (unless (pos-visible-in-window-p p win)
    (set-window-start win p)))

(defun zlc--highlight-nth-completion (n)
  (with-current-buffer "*Completions*"
    (let ((begin (point-min))
          (end (point-min)))
      (dotimes (_ (1+ n))
        (setq begin
              (or (next-single-property-change end 'mouse-face) (point-min)))
        (setq end
              (or (next-single-property-change begin 'mouse-face) (point-max))))
      ;; clear previous highlight
      (zlc--clear-overlay)
      ;; create overlay and set face
      (overlay-put
       (setq zlc--previous-overlay
             (make-overlay begin end))
       'face 'zlc-selected-completion-face)
      ;; ensure highlight is in view
      (zlc--ensure-visible (get-buffer-window) begin))))

;; ============================================================ ;;
;; Public
;; ============================================================ ;;

(defun zlc-reset ()
  (interactive)
  (delete-region zlc--field-begin (field-end))
  (zlc--reset))

(defun zlc-select-next (&optional direction)
  (interactive)
  ;; clear previous completion
  (delete-region zlc--field-begin (field-end))
  ;; set next index
  (incf zlc--index (or direction 1))
  (setq zlc--index
        (cond
         ((>= zlc--index (length zlc--global-cache))
          -1)
         ((< zlc--index 0)
          (if (= zlc--index -1)
              -1                        ; select original string
            (1- (length zlc--global-cache))))
         (t zlc--index)))
  ;; select
  (if (>= zlc--index 0)
      ;; select next completion
      (let* ((str (zlc--current-candidate))
             ;; sometimes (get-text-property 0 'face str) does not work...
             (from (if (eq (cadr (text-properties-at 0 str))
                           'completions-first-difference)
                       0
                     (or (next-property-change 0 str) 0))))
        (insert (substring str from))
        (zlc--highlight-nth-completion zlc--index))
    ;; otherwise
    (zlc--clear-overlay)))

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
select completion orderly."
  (interactive)
  ;; reset when ...
  (unless (or (eq last-command this-command)
              (eq last-command 'zlc-select-previous))
    (setq minibuffer-scroll-window nil))
  (let ((window minibuffer-scroll-window))
    ;; If there's completions, select one of them orderly.
    (if (window-live-p window)
        (or (zlc-select-next 1) t)
      ;; otherwise, reset completions and arrange new one
      (zlc--reset)
      (case (completion--do-completion)
        (#b000 nil)
        (#b001 (goto-char (field-end))
               (minibuffer-message "Sole completion")
               t)
        (#b011 (goto-char (field-end))
               ;; immediately display completions
               (minibuffer-completion-help)
               ;; select first completion if needed
               (when zlc-select-completion-immediately
                 (zlc-select-next 1))
               t)
        (t     t)))))

;; ============================================================ ;;
;; Settings
;; ============================================================ ;;

(let ((map minibuffer-local-map))
  (define-key map (kbd "<backtab>") 'zlc-select-previous)
  ;; (define-key map (kbd "C-c") 'zlc-reset)
  )

(provide 'zlc)
;;; zlc.el ends here