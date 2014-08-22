;; Time-stamp: <2014-03-03 16:41:29 (kolesarm)>

(require 'magit)
(global-set-key (kbd "C-x m") 'magit-status)

;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

;; This code (from sveen) makes magit-status run alone in the frame, and then ;
;; restores the old window configuration when you quit out of magit. No more
;; juggling windows after committing.
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(require 'git-gutter-fringe)
(global-git-gutter-mode t)

;; Show git diff information at right fringe
(setq git-gutter-fr:side 'right-fringe)


(provide 'my-init-magit)
