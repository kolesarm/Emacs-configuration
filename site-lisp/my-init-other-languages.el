;; 1. PYTHON-MODE
(add-hook 'python-mode-hook
          (function (lambda () (setq indent-tabs-mode nil tab-width 4))))

;; For a fully-fledged python environment, try elpy
;; ;; Python completion and code checking
;; (setq elpy-modules '(elpy-module-eldoc
;;                      elpy-module-flymake
;;                      elpy-module-pyvenv
;;                      elpy-module-highlight-indentation
;;                      elpy-module-sane-defaults))
;; (elpy-enable)
;; ;; use ipython if available
;; (if (executable-find "ipython")
;;     (elpy-use-ipython))

;; 2. HASKELL
; http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; 3. MIT-SCHEME
(require 'xscheme)

(provide 'my-init-other-languages)
