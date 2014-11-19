;; Time-stamp: <2014-11-19 15:16:17 (kolesarm)>

;; 1. IBUFFER
(defalias 'list-buffers 'ibuffer)

;; 2. SAVE DESKTOP
;; save a list of open files in ~/.emacs.desktop save the desktop file
;; automatically if it already exists
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; save a bunch of variables to the desktop file for lists specify the
;; length of the maximal saved data also Use M-x desktop-save once to
;; save the desktop. When it exists, Emacs updates it on every exit.
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
  (file-name-history . 100)
  (grep-history . 30)
  (compile-history . 30)
  (minibuffer-history . 50)
  (query-replace-history . 60)
  (read-expression-history . 60)
  (regexp-history . 60)
  (regexp-search-ring . 20)
  (search-ring . 20)
  (shell-command-history . 50)
  tags-file-name
  register-alist)))

;; 3. FULL SCREEN (need to manually download)
;; not needed now with xmonad
;; http://www.emacswiki.org/emacs/fullscreen.el
;;(require 'fullscreen)
;;(global-set-key [f11] 'fullscreen-toggle)

;; 4. YASNIPPET
(require 'yasnippet)
(yas-global-mode t)
; snippets are in .emacs.d/snippets

;; See: http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet

;; 5. HIPPIE-EXPAND
(global-set-key "\M-/" 'hippie-expand)

;; 6. FLYSPELL
;; M-$: correct words (using Ispell).
;; ignore tex sequences
(setq flyspell-issue-welcome-flag nil)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'tex-mode-hook (lambda () (setq ispell-parser 'tex)))
(add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))

;; 7. SMARTPARENS more sophisticated than skeleton-pair,
;; https://github.com/Fuco1/smartparens. Install with package-install.
;; http://www.panix.com/~tehom/my-code/skel-recipe.txt also more sophisticated
;; than autopair.

(smartparens-global-mode t)
;; do not use \" for insert action but use it for any other action in latex mode
(sp-local-pair 'latex-mode "\"" nil :actions '(:rem insert))
(sp-local-pair 'latex-mode "'" nil :actions '(:rem insert))
(sp-local-pair 'latex-mode "`" nil :actions '(:rem insert))

; I'd like to write I'd, not I''d
(sp-pair "'" nil :unless '(sp-point-after-word-p))
(sp-pair "(" nil :unless '(sp-point-before-word-p))
(sp-pair "[" nil :unless '(sp-point-before-word-p))
(sp-pair "\"" nil :unless '(sp-point-before-word-p))

;; smartparens-mode doesn't activate itself in special buffers like R
(add-hook 'ess-R-post-run-hook 'smartparens-mode)

;; 8. CONKEROR AS DEFAULT BROWSER
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/conkeror")

;; Webjumps
(global-set-key (kbd "C-x g") 'webjump)

;; Add Urban Dictionary to webjump
(eval-after-load "webjump"
'(add-to-list 'webjump-sites
              '("Urban Dictionary" .
                [simple-query
                 "www.urbandictionary.com"
                 "http://www.urbandictionary.com/define.php?term="
                 ""])))

;; 9. UNFILL PARAGRAPH
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; 13. LINE NUMBERS
;; only display line numbers temporarily
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(provide 'my-init-other)
