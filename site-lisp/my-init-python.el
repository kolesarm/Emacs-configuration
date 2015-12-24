;; 1. PYTHON-MODE
(add-hook 'python-mode-hook
          (function (lambda () (setq indent-tabs-mode nil tab-width 4))))

;; 2. HASKELL
; http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; 3. MIT-SCHEME
(require 'xscheme)

(provide 'my-init-python)
