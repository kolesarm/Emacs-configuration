;; Time-stamp: <2014-02-25 18:51:57 (kolesarm)>

;; 1. EMACS LOAD PATH. We install all packages in ~/.emacs.d/site-lisp
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
;; Emacs Wiki:
;; This way of specifying it automatically adds subdirectories. Since
;; various packages store information in ~/.emacs.d/, it is unwise to
;; add all of its sub-directories to ‘load-path’. Above we only added
;; the sub-directory site-lisp to avoid loading files that aren’t
;; libraries. See http://www.emacswiki.org/emacs/LoadPath

;; 2. TURN OFF MOUSE INTERFACE early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode 1)) Menu is ok

(setq inhibit-startup-message t) ; no splash screen
(setq inhibit-startup-echo-area-message "kolesarm") ; load quietly

;; 3. PACKAGES IN EMACS 24
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; 4. CUSTOM SCRIPTS
;; Custom scripts are all in ~/.emacs.d/site-lisp/myplugins/

;; Change defaults and appearance
(require 'my-defaults) ; default variable settings
(require 'my-appearance)

;; Setup extensions
(require 'my-init-other) ;; misc extensions
(require 'my-ido) ; ido and recentf

;; Language-specific
(load "my-init-python") ; python, sage, haskell
(load "my-init-magit")

;; need to updated these three
(load "my-init-matlab")
(load "my-init-latex")
(eval-after-load 'org '(require 'my-org)) ; need to update

 ;; (require 'setup-mu4e) read sveen's

; 4. INITIAL BUFFER
(setq initial-buffer-choice "/home/kolesarm/backuplog.org")
