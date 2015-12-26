; Time-stamp: <2015-12-26 14:31:15 (kolesarm)>

;; Set email to my actual email, not @fisher203.princeton.edu or whatever
(setq user-mail-address "kolesarmi@googlemail.com")
(setq user-full-name "Michal Kolesár")

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions. The saveplace package is part of Emacs
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Allow copying and pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(add-hook 'before-save-hook (set-buffer-file-coding-system 'utf-8))

;; Don't remove text in active region if inserting text
(delete-selection-mode -1)

;; Lines should be 80 characters wide, not 72
(set-default 'fill-column 80)

;; WINNER-MODE: Undo/redo window configuration with C-c <left>/<right>
;; Part of Emacs 23.2 and up
(winner-mode t)

;; WINDMOVE comes with Emacs21. WINDOW-NUMBERING is better, part of MELPA
(require 'window-numbering)

;; Solarized base03 color
(set-face-attribute 'window-numbering-face nil
                    :foreground "#002b36" :weight 'bold)
(window-numbering-mode 1)

;; Move cursor into between CamelCaseWords, since Emacs 23.2
(global-subword-mode t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique.
;; Distributed with Emacs
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Narrowing allowed
(put 'narrow-to-region 'disabled nil)

;; Whitespace

;; Use only spaces and no tabs
(setq-default indent-tabs-mode nil)

;; from MELPA
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)
(setq mode-require-final-newline nil)
;; Handles "require-final-newline", "show-trailing-whitespace".
;; Ending file w  newline interferes with yasnippet, there it's ok

(setq
  time-stamp-active t          ; do enable time-stamps
  time-stamp-line-limit 50     ; check first 50 buffer lines for Time-stamp:
  time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'before-save-hook 'time-stamp) ;; time-stamp all files

;; Ask for confirmation before quitting Emacs
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "You really want to kill me?"))
          'append)

(provide 'my-defaults)
;;; my-defaults.el ends here
