;; Time-stamp: <2015-12-26 09:36:39 (kolesarm)>

;; 1. IDO-MODE (included with emacs23)
;; Interactively DO things:
;; improves functionality of C-x C-f and C-x b
;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
(require 'ido)
(ido-mode t)

(setq ido-enable-flex-matching t  ; fuzzy matching
      ido-everywhere t
      ido-use-virtual-buffers t
      ido-use-filename-at-point nil ; don't use filename at point (annoying)
      ido-use-url-at-point nil ; don't use url at point (annoying)
      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
      ido-enable-last-directory-history t ; remember last used dirs
      ido-max-work-directory-list 30 ; should be enough
      ido-max-work-file-list 50 ; remember many
      ido-max-prospects 6 ; don't spam my minibuffer
      ido-confirm-unique-completion t ; wait for RET, even with unique completion
      ido-create-new-buffer 'always ; create new buffer if no match
      )

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; Completion hints for files and buffers buffers
(setq ido-file-extensions-order '(".tex" ".sty" ".R" ".r" ".sh" ".bib" ".org"
                                  ".py" ".el" ".pdf"
                                  ".txt" ".html"))

;; C-x C-f runs the command ibuffer-find-file in an ibuffer by default. Here we
;; make it use ido-find-file
(defun ibuffer-ido-find-file (file &optional wildcards)
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory
           (let ((buf (ibuffer-current-buffer)))
             (if (buffer-live-p buf)
                 (with-current-buffer buf
                   default-directory)
               default-directory))))
     (list (ido-read-file-name "Find file: " default-directory) t)))
  (find-file file wildcards))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-key ibuffer-mode-map "\C-x\C-f"
              'ibuffer-ido-find-file)))

;; ~/ sends you home.

;; 2. RECENTF (included with emacs21)
;;recentf M-F9 will open a buffer with recently open files
;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/
(recentf-mode t)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key [(meta f9)] 'recentf-open-files)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; get rid of `find-file-read-only' C-x C-r binding, and replace with
;; something more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; Disable undo in Buffer-mode
(add-hook 'Buffer-menu-mode-hook 'buffer-disable-undo)

;; 3. BROWSE KILL RING
(require 'browse-kill-ring)

;; M-y key binding will activate browse-kill-ring iff the normal behavior is not
;; available, i.e., when the last command was not a 'yank'.
(browse-kill-ring-default-keybindings)

(provide 'my-ido)
