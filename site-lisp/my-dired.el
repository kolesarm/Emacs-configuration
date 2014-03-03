;; Time-stamp: <2014-03-03 15:54:42 (kolesarm)>

;; I want C-x C-j to run dired-jump, it's in dired-x
(require 'dired-x)

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

(provide 'my-dired)
