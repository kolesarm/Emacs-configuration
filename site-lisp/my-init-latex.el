;; 1. AUCTEX

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)        ; Enable parse on save.
(setq TeX-parse-self t)       ; Parse file after loading it if no style hook is found
                              ; for it.
(setq-default TeX-master nil) ; master file in multi-file
(setq TeX-save-query nil)     ; If non-nil, then query user before saving each
                              ; file with TeX-save-document.
(setq TeX-PDF-mode t)         ; pdf mode by default

; don't highlight syntax inside these
(setq LaTeX-verbatim-environments '("Verbatim" "lstlisting"))
(setq LaTeX-verbatim-macros-with-delims '("lstinline" "verb" "verb*"))

;; customize viewers
(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince %o")
        ("^html?$" "." "iceweasel %o"))))

;; auto-fill and LaTeX-math-mode by default in TeX-mode
(add-hook 'LaTeX-mode-hook
 (lambda ()
   (LaTeX-math-mode)
   (turn-on-reftex)
   (turn-on-auto-fill)))


;; reftex settings
(setq reftex-plug-into-auctex t)

;; do not prompt whether I want \pageref or not.
(setq reftex-ref-macro-prompt nil)

(eval-after-load
    "latex"
  '(TeX-add-style-hook
    "cleveref"
    (lambda ()
      (if (boundp 'reftex-ref-style-alist)
      (add-to-list
       'reftex-ref-style-alist
       '("Cleveref" "cleveref"
         (("\\Cref" ?C) ("\\cref" ?c) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
      (add-to-list 'reftex-ref-style-default-list "Cleveref")
      (setq reftex-label-alist '(AMSTeX)) ; not sure why it doesn't parse amstex
                                        ; automatically
      (TeX-add-symbols
       '("cref" TeX-arg-ref)
       '("Cref" TeX-arg-ref)
       '("cpageref" TeX-arg-ref)
       '("Cpageref" TeX-arg-ref)))))


;; 2. EXTRA SHORTCUTS
(load "my-latex-primer")

;; Inserts {} automaticly on _ and ^ (no need for custom command)
(setq TeX-electric-sub-and-superscript t)


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


;; List of directories to search for AUCTeX style files. Each must end with a
;; slash.
(setq TeX-style-path '("/var/lib/auctex/emacs24" ;
                       "/usr/share/emacs/site-lisp/auctex/style" ;
                       "/home/kolesarm/.emacs.d/site-lisp/auctex-styles/"
                       "auto/"
                       "style/"))

;; 3 Make C-c-c not ask, just do the default action. Add C-c-a for asking
(setq TeX-command-force "")
;; If non-nil, TeX-command-query will return the value of this variable instead
;; of quering the user.

(add-hook 'LaTeX-mode-hook
          '(lambda()
             (define-key LaTeX-mode-map "\C-c\C-a" ; 'a' for ask
               (lambda (arg) (interactive "P")
                 (let ((TeX-command-force nil))
                   (TeX-command-master arg))))))


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
                          "disabled"))))

(add-hook 'LaTeX-mode-hook
          (lambda nil
            (local-set-key (kbd "C-c C-t x") 'TeX-toggle-escape)))


;; 5. latexmk and arara

;; Add latexmk option to TeX-command-list and make it default
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'TeX-expand-list
                         '("%(tex-file-name)"
                           (lambda ()
                             (concat
                              "\"" (car (split-string (buffer-file-name) "\\.Rnw"))
                              ".tex" "\""))))
            (push
             '("LaTeXmk" "latexmk -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk on file") TeX-command-list)
            (push
             '("knitr" "R -e 'knitr::knit(\"%s\")'" TeX-run-TeX nil t
               :help "Run knitr on file") TeX-command-list)
            (push
             '("klatex" "R -e 'knitr::knit(\"%s\")';latexmk -pdf %(tex-file-name)"
               TeX-run-TeX nil t
               :help "Run knitr and latexmk on .tex") TeX-command-list)
            (push
             '("arara" "arara --verbose %s" TeX-run-TeX nil t
               :help "Run arara on file") TeX-command-list)
            (push
             '("compileboth" "compileboth %s" TeX-run-TeX nil t
               :help "Generate questions and answers") TeX-command-list)
            (setq TeX-command-default "LaTeXmk")))

(provide 'my-init-latex)
