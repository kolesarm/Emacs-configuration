;; 1. MATLAB
(load-library "matlab-load")            ; in MELPA

;; customization
(setq matlab-indent-level 4)
(setq matlab-highlight-block-match-flag t)
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))

; enable CEDET---not sure what it's useful for
(matlab-cedet-setup)

; matlab-mode has support for Completion engine. Using tools in CEDET, Emacs
; will parse your M files and provide pretty good completion and code
; decoration.

; enable MLINT
(setq mlint-programs (quote ("/usr/local/MATLAB/R2013b/bin/glnxa64/mlint")))
(add-hook 'matlab-mode-hook (lambda () (mlint-minor-mode 1)))
(setq matlab-show-mlint-warnings t)
(setq matlab-highlight-cross-function-variables t)

; Customize comment string to '%' only rather than '% $$$'
(setq matlab-comment-region-s "% ")

; 2. ESS: R, Stata, SAS
(require 'ess-site)

;; Start R in the working directory by default
(setq ess-ask-for-ess-directory nil)

(setq ess-ask-about-transfile nil)
; If set to t, ESS asks where to save the text in the buffer with R process. You
; can number these files according to date, so you will always have another way
; to track what exactly you were doing.

(set-default 'ess-history-file nil) ; don't save history

(provide 'my-init-matlab)
