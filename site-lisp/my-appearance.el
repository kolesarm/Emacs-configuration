;; Time-stamp: <2015-12-23 16:17:17 (kolesarm)>

;; 1. FONT LOCK AND BELLS

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Solid, rather than blinking cursor
(blink-cursor-mode -1)

;; Highlight current line
(global-hl-line-mode 1)

;; Show active region
(transient-mark-mode t)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Display glyph in the fringe of each empty line at the end of the buffer
(setq-default indicate-empty-lines t)

;; visible, rather than audible bell
(setq visible-bell t)

;; 2. PAREN-MODE
;; http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html
;; Highlight matched/mismatched paren
(show-paren-mode 1)                     ;turn paren-mode on
(setq show-paren-delay 0)               ;deactivate the delay

;; Highlight the whole expression not just the parentheses. If set to
;; parenthesis, only corresponding parentheses will be highlighted
(setq show-paren-style 'expression)

;; 2.2 Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error)

(setq rainbow-delimiters-max-face-count 1)

;; 3. COLORS AND FONTS

;; Prior to Emacs 24, use color-theme. Now install color-theme-solarized, simply
(load-theme 'solarized t) ; M-x disable-theme disables it
;; Used to be solarized-light


; Anonymous pro is designed for coding. O,0 and 1,l look distinct. Source Code
; Pro is an alternative: (set-frame-font "Source Code Pro-16" nil t)
(when (window-system)
  (set-frame-font "Anonymous Pro")
  (set-face-attribute 'default nil :family "Anonymous Pro" :height 130)
  (set-face-font 'default "Anonymous Pro"))

; fall back on DejaVu for unicode characters
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 12.0
                               :weight 'normal)))

;; 4. ANZU MODE for displaying number of seach matches
(require 'anzu)
(global-anzu-mode t)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "dark slate gray" :weight 'bold)

;; 4. UNCLUTTER THE MODELINE
(require 'diminish)

(eval-after-load "yasnippet" '(diminish 'yas-minor-mode)) ;; Yas
(eval-after-load "ethan-wspace" '(diminish 'ethan-wspace-mode)) ;; ew:mnlt
(eval-after-load "flyspell" '(diminish 'flyspell-mode)) ;; Fly
(eval-after-load "git-gutter-fringe" '(diminish 'git-gutter-mode)) ;; GitGutter
(eval-after-load "anzu-mode" '(diminish 'anzu-mode)) ;;


(provide 'my-appearance)

;; 5. Reset font lock:   (normal-mode) (font-lock-fontify-buffer)
