;; Time-stamp: <2019-02-04 11:10:13 (kolesarm)>

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
(require 'ob-stata)                       ; Org-Babel support for evaluating stata code.
(eval-after-load 'org '(require 'my-org)) ; org and todotxt
(require 'my-init-flycheck)                ; flycheck


;; 4. INITIAL BUFFER
(setq initial-buffer-choice "/home/kolesarm/backuplog.org")

;; 5. START SERVER
(server-start)
;; Once Emacs server is started, you can use a shell command called emacsclient
;; to connect to the Emacs process: set $EDITOR to it, or to emacsclient -c, or
;; in particular, ~/.gitconfig wants emacsclient as editor (and not emacsclient
;; -nw or emacs -nw), otherwise magit complains

;; 6. PREVENT EMACS FROM FREEZING WHEN YANKING FROM X11
(setq x-selection-timeout 10)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (smartparens pdf-tools polymode markdown-mode julia-mode langtool yasnippet window-numbering todotxt-mode smex rainbow-delimiters matlab-mode magit git-gutter-fringe flycheck ethan-wspace ess diminish color-theme-solarized browse-kill-ring anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
