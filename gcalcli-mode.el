;;; gcalcli-mode.el --- view gcalcli output in emacs

;; Copyright (C) 2020 by Ric Lister

;; Author: Ric Lister
;; URL: https://github.com/rlister/gcalcli-mode
;; Version: 20220324
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; Minimalist viewer for output of gcalcli tool to view Google Calendar in emacs.
;;
;; Usage:
;;
;; Add the following to your emacs config:
;;
;; (add-to-list 'load-path "~/path/to/gcalcli-mode")
;; (autoload 'gcalcli-agenda "gcalcli-agenda" nil t)

(define-derived-mode gcalcli-mode special-mode "GCal")

(defvar gcalcli-bin (executable-find "gcalcli")
  "Location of gcalcli command.")

(defvar gcalcli-config-alist
  '((nil . nil))
  "List of name and config-folder pairs, if using multiple accounts.")

(defvar-local gcalcli--config-folder nil
  "Buffer local config-folder, if set.")

(defvar-local gcalcli--details nil
  "Buffer local agenda details to show.")

(defvar-local gcalcli--start-offset nil
  "Days ahead of current time to start agenda.")

(defun gcalcli-read-config ()
  "If gcalcli-config-alist has more than one entry, ask user which to use, and return alist cdr."
  (if (< (length gcalcli-config-alist) 2)
      (car gcalcli-config-alist)
    (assq
     (intern (completing-read "gcalcli config: " (mapcar 'car gcalcli-config-alist)))
     gcalcli-config-alist)))

(defun gcalcli-make-cmd ()
  "Construct shell command and args to run gcalcli agenda."
  (let ((args
         (append
          (when gcalcli--config-folder
            (list "--config-folder" gcalcli--config-folder))
          (list "agenda")
          (when gcalcli--details
            (list "--details" gcalcli--details))
          (when gcalcli--start-offset
            (list (format-time-string "%Y-%m-%d" (time-add (current-time) (* gcalcli--start-offset 24 3600))))))))
    (mapconcat 'identity (cons gcalcli-bin args) " ")))

(defun gcalcli-insert-agenda ()
  "Run gcalcli and use output to replace current buffer content."
  (with-temp-message "Updating agenda..."
    (save-excursion
      (let ((buffer-read-only nil)
            (cmd (gcalcli-make-cmd)))
        (erase-buffer)
        (insert (format-time-string "updated %c\n"))
        (insert (format ";; %s\n\n" cmd))
        (insert (shell-command-to-string cmd))
        (ansi-color-apply-on-region (point-min) (point-max))))))

(defun gcalcli-refresh ()
  "Call gcalcli to update buffer."
  (interactive)
  (gcalcli-insert-agenda))

(defun gcalcli-set-details (str)
  "Set value of --details agenda arg; with new value will overwrite, with same value will toggle."
  (if (string= gcalcli--details str)
      (setq gcalcli--details nil)
    (setq gcalcli--details str))
  (gcalcli-refresh))

(defun gcalcli-toggle-location ()
  "Show/unshow event locations."
  (interactive)
  (gcalcli-set-details "location"))

(defun gcalcli-toggle-conference ()
  "Show/unshow event conference links."
  (interactive)
  (gcalcli-set-details "conference"))

;; recommended to use --lineart=ascii in config file
(defun gcalcli-toggle-description ()
  "Show/unshow event description."
  (interactive)
  (gcalcli-set-details "description"))

(defun gcalcli-toggle-all ()
  "Show/unshow all event fields."
  (interactive)
  (gcalcli-set-details "all"))

(defun gcalcli-incr-start-offset (days)
  "Increase start offset by given number of days, or nil to reset."
  (if days
      (let ((current (or gcalcli--start-offset 0)))
        (setq gcalcli--start-offset (+ current days)))
    (setq gcalcli--start-offset nil))
  (gcalcli-refresh))

(defun gcalcli-agenda-later (&optional arg)
  "Go forward in time by the current span. With ARG, do it that many times."
  (interactive "p")
  (gcalcli-incr-start-offset (* 7 (or arg 1))))

(defun gcalcli-agenda-earlier (&optional arg)
  "Go backward in time by the current span. With ARG, do it that many times."
  (interactive "p")
  (gcalcli-incr-start-offset (* -7 (or arg 1))))

(defun gcalcli-agenda-today ()
  "Reset view to current agenda span."
  (interactive)
  (gcalcli-incr-start-offset nil))

(defun gcalcli-make-add-cmd ()
  "Construct shell command and args to run gcalcli add."
  (append
   (when gcalcli--config-folder
     (list "--config-folder" gcalcli--config-folder))
   (list "add")))

(defun gcalcli-add ()
  "Call gcalcli add command for current config."
  (interactive)
  (pop-to-buffer
   (apply #'make-comint "gcalcli-add" gcalcli-bin nil (gcalcli-make-add-cmd))))

(defun gcalcli-make-edit-delete-cmd (cmd)
  "Construct shell command and args to run gcalcli edit or delete."
  (let ((text (read-string "text: ")))
    (append
     (when gcalcli--config-folder
       (list "--config-folder" gcalcli--config-folder))
     (list cmd)
     (list text))))

(defun gcalcli-edit ()
  "Call gcalcli edit command for current config."
  (interactive)
  (pop-to-buffer
   (apply #'make-comint "gcalcli-edit" gcalcli-bin nil (gcalcli-make-edit-delete-cmd "edit"))))

(defun gcalcli-delete ()
  "Call gcalcli delete command for current config."
  (interactive)
  (pop-to-buffer
   (apply #'make-comint "gcalcli-delete" gcalcli-bin nil (gcalcli-make-edit-delete-cmd "delete"))))

(defun gcalcli-agenda-buffer-name (&optional key)
  "Build name for agenda buffer."
  (if key
      (format "*gcalcli: %s*" key)
    "*gcalcli*"))

;;;###autoload
(defun gcalcli-agenda ()
  "Display gcalcli agenda."
  (interactive)
  (let* ((cfg (gcalcli-read-config))
         (path (cdr cfg)))
    (switch-to-buffer (gcalcli-agenda-buffer-name (car cfg)))
    (gcalcli-mode)
    (setq gcalcli--config-folder (and path (expand-file-name path)))
    (gcalcli-insert-agenda)
    (visual-line-mode -1)))

(defvar gcalcli-mode-map (make-sparse-keymap)
  "Keymap for gcalcli-mode.")

(define-key gcalcli-mode-map (kbd "n") 'next-line)
(define-key gcalcli-mode-map (kbd "p") 'previous-line)
(define-key gcalcli-mode-map (kbd "g") 'gcalcli-refresh)
(define-key gcalcli-mode-map (kbd "l") 'gcalcli-toggle-location)
(define-key gcalcli-mode-map (kbd "c") 'gcalcli-toggle-conference)
(define-key gcalcli-mode-map (kbd "d") 'gcalcli-toggle-description)
(define-key gcalcli-mode-map (kbd "A") 'gcalcli-toggle-all)
(define-key gcalcli-mode-map (kbd "f") 'gcalcli-agenda-later)
(define-key gcalcli-mode-map (kbd "b") 'gcalcli-agenda-earlier)
(define-key gcalcli-mode-map (kbd "t") 'gcalcli-agenda-today)
(define-key gcalcli-mode-map (kbd "a") 'gcalcli-add)
(define-key gcalcli-mode-map (kbd "e") 'gcalcli-edit)
(define-key gcalcli-mode-map (kbd "C-k") 'gcalcli-delete)

(require 'ansi-color)
(provide 'gcalcli-mode)
;;; gcalcli-mode.el ends here
