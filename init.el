;; Time-stamp: <2016-02-19 18:04:30 (kolesarm)>

;; 1. EMACS LOAD PATH. All custom code in ~/.emacs.d/site-lisp
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; This way of specifying it automatically adds subdirectories. Since various
;; packages store information in ~/.emacs.d/, it is unwise to add all of its
;; sub-directories to ‘load-path’. Above we only added the sub-directory
;; site-lisp to avoid loading files that aren’t libraries. It avoids
;; subdirectories not starting with letters or digits, those named RCS or CVS,
;; and those containing a file named .nosearch. See
;; http://www.emacswiki.org/emacs/LoadPath

;; 2. TURN OFF MOUSE INTERFACE early in startup to avoid momentary display,
;; menu-bar-mode is OK
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; no splash screen
(setq inhibit-startup-message t)
;; inhibit echo area at startup
(setq inhibit-startup-echo-area-message "kolesarm")

;; 3. PACKAGES IN EMACS 24
(require 'package)                      ; load package manager
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Make a list of the packages you want
(setq my-package-list '(;; gnu packages: auctex melpa packages:
                        anzu
                        browse-kill-ring
                        color-theme-solarized
                        diminish
                        ethan-wspace
                        flycheck
                        git-gutter-fringe
                        markdown-mode
                        matlab-mode
                        smartparens
                        smex
                        polymode
                        todotxt-mode
                        window-numbering
                        yasnippet))
(package-initialize)                    ; initialize ELPA packages

;; 4. CUSTOM SCRIPTS
;; Change defaults and appearance

(require 'my-defaults)                  ; default variable settings
(require 'my-appearance)

;; Setup extensions
(require 'my-init-other)                ; misc extensions
(require 'my-ido)                       ; ido, recentf, browse-kill-ring
(require 'my-dired)                     ; Directory Editor

;; Language-specific
(require 'my-init-magit)
(require 'my-init-other-languages)        ; python, haskell, scheme
(require 'my-init-latex)                  ; latex
(require 'my-init-matlab)                 ; matlab and ESS
(require 'ob-stata)               ; Org-Babel support for evaluating stata code.
(eval-after-load 'org '(require 'my-org)) ; org and todotxt
(require 'my-init-flymake)                ; flymake


;; 4. INITIAL BUFFER
(setq initial-buffer-choice "/home/kolesarm/backuplog.org")

;; 5. START SERVER
(server-start)

;; 6. TELL ME ABOUT PACKAGES
(if (every #'package-installed-p my-package-list)
    (message "All packages installed")
  (display-warning :warning "Not all packages installed"))
