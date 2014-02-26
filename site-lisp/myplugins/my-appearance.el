;; Time-stamp: <2014-02-25 19:45:43 (kolesarm)>

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

;; Show me empty lines after buffer end
(setq-default indicate-empty-lines t)

;; visible, rather than audible bell
(setq visible-bell t)

;; 2. PAREN-MODE
;; http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html
(show-paren-mode 1) ;turn paren-mode on
(setq show-paren-delay 0) ;deactivate the delay

;; Highlight the whole expression not just the parentheses. If set to
;; parenthesis, only corresponding parentheses will be highlighted
(setq show-paren-style 'expression)

;; 3. COLORS AND FONTS
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-wheat)

; Anonymous pro is designed for coding. O,0 and 1,l look distinct.
(when (window-system)
  (set-frame-font "Anonymous Pro")
  (set-face-attribute 'default nil :family "Anonymous Pro" :height 135)
  (set-face-font 'default "Anonymous Pro"))

; fall back on DejaVu for unicode characters
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 12.0
                               :weight 'normal)))

;; 4. UNCLUTTER THE MODELINE
(require 'diminish)

(eval-after-load "yasnippet" '(diminish 'yas-minor-mode)) ;; Yas
(eval-after-load "ethan-wspace" '(diminish 'ethan-wspace-mode)) ;; ew:mnlt
(eval-after-load "flyspell" '(diminish 'flyspell-mode)) ;; Fly


(provide 'my-appearance)
