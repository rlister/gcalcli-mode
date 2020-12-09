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
          )))
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

(provide 'gcalcli-mode)
;;; gcalcli-mode.el ends here
