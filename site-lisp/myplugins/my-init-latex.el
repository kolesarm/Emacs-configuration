;; 1. AUCTEX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)        ; Enable parse on save.
(setq TeX-parse-self t)       ; Parse file after loading it if no style hook is found
                              ; for it.
(setq-default TeX-master nil) ; master file in multi-file
(setq TeX-save-query nil)     ; If non-nil, then query user before saving each
                              ; file with TeX-save-document.

(setq TeX-PDF-mode t) ; pdf mode by default

;; customize viewers
; okular -unique emacs.pdf#src:49emacs.tex
;(setq TeX-view-program-list '(("evince" "evince --page-index=%(outpage) %o")))
;; use evince for dvi and pdf viewer
;; evince-dvi backend should be installed
;(setq TeX-view-program-selection
;      '((output-dvi "DVI Viewer")
;        (output-pdf "PDF Viewer")
;        (output-html "Google Chrome")))
;(setq TeX-view-program-list
;      '(("DVI Viewer" "evince %o")
;        ("PDF Viewer" "evince --page-index=%(outpage) %o")))
; AUCTeX 11.85 does this differently than later versions
(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince %o")
        ("^html?$" "." "conkeror %o"))))

; Auctex 11.85 doesn't have this
; (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
; (setq TeX-source-correlate-start-server t)

;; auto-fill and LaTeX-math-mode by default in TeX-mode
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq reftex-plug-into-auctex t)

;; 2. EXTRA SHORTCUTS
(load "my-latex-primer")

;Flymake is one of those packages. It enables Emacs to check the syntax of your
;TeX file on-the-fly. To turn it on, put the following code in your
;$HOME/.emacs:
;http://soundandcomplete.com/2010/05/13/emacs-as-the-ultimate-latex-editor/

;; 3. New stuff--everything above just copied from old .emacs
;; 3.1 STYLE FILES
(setq TeX-style-path '("/var/lib/auctex/emacs23"
                      "/usr/share/emacs/23.2/site-lisp/auctex/style"
                      "/home/kolesarm/.emacs.d/auctex/auto"
                      "/home/kolesarm/.emacs.d/auctex/style"
                      "/home/kolesarm/.emacs.d/site-lisp/myplugins/auctex-styles"
                      "auto"
                      "style"))
;; 3.2 Inserts {} automaticly on _ and ^ (no need for custom command)
(setq TeX-electric-sub-and-superscript t)

;; 3.3 Make C-c-c not ask, just do the default action. Add C-c-a for asking
(setq TeX-command-force "")
;; If non-nil, TeX-command-query will return the value of this
;; variable instead of quering the user.

(add-hook 'LaTeX-mode-hook
          '(lambda()
             (define-key LaTeX-mode-map "\C-c\C-a" ; 'a' for ask
               (lambda (arg) (interactive "P")
                 (let ((TeX-command-force nil))
                   (TeX-command-master arg))))))


;; 3.4 Customize Latex-math-list
(defun LaTeX-math-overbar ()
  "Insert \\overbar{}."
  (interactive)
  (insert "\\overbar{}")
  (backward-char 1))
(defun LaTeX-math-underbar ()
  "Insert \\ubar{}."
  (interactive)
  (insert "\\ubar{}")
  (backward-char 1))

(defun LaTeX-math-hatbrace ()
  "Insert \\hat{}."
  (interactive)
  (insert "\\hat{}")
  (backward-char 1))

(defun LaTeX-math-tildebrace ()
  "Insert \\tilde{}."
  (interactive)
  (insert "\\tilde{}")
  (backward-char 1))

(defun LaTeX-math-Beta ()
  "Insert \\Beta."
  (interactive)
  (insert "\\Beta"))

(defun LaTeX-math-Eta ()
  "Insert \\Eta."
  (interactive)
  (insert "\\Eta"))

(setq LaTeX-math-list '((?_ LaTeX-math-underbar nil)
                        (?- LaTeX-math-overbar nil)
                        (?~ LaTeX-math-tildebrace nil)
                        (?B LaTeX-math-Beta nil)
                        (?H LaTeX-math-Eta nil)
                        (?^ LaTeX-math-hatbrace nil)))


;; 4. Shell escape
;; toggle shell escape using C-c C-t x
(defun TeX-toggle-escape nil (interactive)
"Toggle Shell Escape"
(setq LaTeX-command
  (if (string= LaTeX-command "latex") "latex -shell-escape"
    "latex"))
(message (concat "shell escape "
         (if (string= LaTeX-command "latex -shell-escape")
         "enabled"
           "disabled"))
     ))
(add-hook 'LaTeX-mode-hook
      (lambda nil
    (local-set-key (kbd "C-c C-t x") 'TeX-toggle-escape)))

;; 5. latexmk

;; Add latexmk option to TeX-command-list and make it default
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("LaTeXmk" "latexmk -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk on file") TeX-command-list)
            (setq TeX-command-default "LaTeXmk")))


;; 6. Sweave

;; this line doesn't seem to do anything...
(setq ess-swv-plug-into-AUCTeX-p t)

(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.Snw\\'" . Snw-mode))

;;  Change TeX-master-file to .tex, copied from definition of function
;;  TeX-command-master
(defun Rnw-command-master (&optional override-confirm)
  "Run command on the current document.

If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
depend on it being positive instead of the entry in `TeX-command-list'."
  (interactive "P")
  (let ((TeX-transient-master (concat
                               (file-name-sans-extension (buffer-file-name))
                               ".tex"))
        (TeX-master nil))
    (TeX-command (TeX-command-query (TeX-master-file)) 'TeX-master-file
                 override-confirm)))


; I think new ess does this differently
;(define-key noweb-minor-mode-map (kbd "\C-c \C-c") 'Rnw-command-master)

;; Add Sweave to options, can access through C-c C-a, since C-c C-c does its
;; automatic thing
(add-hook 'Rnw-mode-hook
          (lambda ()
            (add-to-list 'TeX-command-list
                         '("Sweave" "R CMD Sweave %s"
                           TeX-run-command nil t :help "Run Sweave") t)
            (setq TeX-command-default "Sweave")))



(provide 'my-init-latex)
