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
  "Run gcalcli and use output to replace currentbuffer content."
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

(defun gcalcli-incr-start-offset (days)
  "Increase start offset by given number of days, or nil to reset."
  (if days
      (let ((current (or gcalcli--start-offset 0)))
        (setq gcalcli--start-offset (+ current days)))
    (setq gcalcli--start-offset nil))
  (gcalcli-refresh))

(defun gcalcli-agenda-later ()
  "Go forward in time by the current span."
  (interactive)
  (gcalcli-incr-start-offset 7))

(defun gcalcli-agenda-earlier ()
  "Go backward in time by the current span."
  (interactive)
  (gcalcli-incr-start-offset -7))

(defun gcalcli-agenda-today ()
  "Reset view to current agenda span."
  (interactive)
  (gcalcli-incr-start-offset nil))

;;;###autoload
(defun gcalcli-agenda ()
  "Display gcalcli agenda."
  (interactive)
  (let ((cfg (gcalcli-read-config)))
    (switch-to-buffer (format "*gcal %s*" (car cfg)))
    (gcalcli-mode)
    (setq gcalcli--config-folder (cdr cfg))
    (gcalcli-insert-agenda)
    (visual-line-mode -1)))

(defvar gcalcli-mode-map (make-sparse-keymap)
  "Keymap for gcalcli-mode.")

(define-key gcalcli-mode-map (kbd "n") 'next-line)
(define-key gcalcli-mode-map (kbd "p") 'previous-line)
(define-key gcalcli-mode-map (kbd "g") 'gcalcli-refresh)
(define-key gcalcli-mode-map (kbd "l") 'gcalcli-toggle-location)
(define-key gcalcli-mode-map (kbd "f") 'gcalcli-agenda-later)
(define-key gcalcli-mode-map (kbd "b") 'gcalcli-agenda-earlier)
(define-key gcalcli-mode-map (kbd "t") 'gcalcli-agenda-today)

(provide 'gcalcli-mode)
;;; gcalcli-mode.el ends here
