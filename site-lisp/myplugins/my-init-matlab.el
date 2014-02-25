;; CEDET
;; first download and install CEDET
;; download form http://sourceforge.net/projects/cedet/
;; compile

;; MATLAB
;; now download from http://sourceforge.net/projects/matlab-emacs/
;; compile
(load-library "matlab-load")

;; matlab with .mod dynare files
(add-to-list 'auto-mode-alist '("\\.mod$" . matlab-mode))

;; customization
(setq matlab-indent-level 4)
(setq matlab-highlight-block-match-flag t)
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))

; enable CEDET---not sure what it's useful for
(matlab-cedet-setup)
;; CEDET
;(load-file "~/.emacs.d/site-lisp/cedet-1.0/common/cedet.el")

; matlab-mode has support for Completion engine. Using tools in CEDET, Emacs
; will parse your M files and provide pretty good completion and code
; decoration.

; enable MLINT
(setq mlint-programs (quote ("/opt/matlab/bin/glnxa64/mlint")))
(add-hook 'matlab-mode-hook (lambda () (mlint-minor-mode 1)))
(setq matlab-show-mlint-warnings t)
(setq matlab-highlight-cross-function-variables t)

; Customize comment string to '%' only rather than '% $$$'
(setq matlab-comment-region-s "% ")

; ESS
(require 'ess-site)
(setq ess-ask-about-transfile nil)
(set-default 'ess-history-file nil) ; don't save history


; If set to t, ESS asks where to save the text in the buffer with R process. You
; can number these files according to date, so you will always have another way
; to track what exactly you were doing.
