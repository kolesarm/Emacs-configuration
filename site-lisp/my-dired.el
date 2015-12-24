;; Time-stamp: <2015-12-24 04:03:45 (kolesarm)>

;; I want C-x C-j to run dired-jump, it's in dired-x
(require 'dired-x)

(put 'dired-find-alternate-file 'disabled nil) ; don't open a new dired buffer
                                               ; i.e. enable `a' command

;; Reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
        (eval `(defadvice ,it (after revert-buffer activate)
                 (revert-buffer))))

;; Make sizes human-readable by default, sort version numbers correctly, and put
;; dotfiles and capital-letters first.
(setq-default dired-listing-switches "-alhv")

;; make sure dired buffers end in a slash so we can identify them easily
(defun ensure-buffer-name-ends-in-slash ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat name "/") t))))
(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)
(add-hook 'dired-mode-hook
          (lambda()
            (setq truncate-lines 1)))

;; open files in external programs
;; (from http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun xah-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.

  The app is chosen from your OS's preference."
  (interactive)
  (let (doIt
        (myFileList
         (cond
          ((string-equal major-mode "dired-mode")
           (dired-get-marked-files))
          ((not file) (list (buffer-file-name)))
          (file (list file)))))
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ")))
    (when doIt
      (cond
       ((string-equal system-type "gnu/linux")
         (mapc
          (lambda (fPath)
            (let ((process-connection-type nil))
              (start-process "" nil "xdg-open" fPath))) myFileList))))))

(define-key dired-mode-map (kbd "E") 'xah-open-in-external-app)

(provide 'my-dired)
