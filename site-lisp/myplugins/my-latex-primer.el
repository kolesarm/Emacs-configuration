;; Based on LaTeX Primer and cdlatex.el

;; This file changes some keybindings in LaTeX Primer and added some
;; functionality from cdlatex ( symbols "^", "_", "{" and "[" now
;; redefined)

;; LaTeX Primer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special math alphabet for LaTeX (activated by ctrl+')
;; By Joakim Ahnfelt-Rønne (October 2006)
;;
;; You may freely use, modify and distribute this file (on
;; your own peril) as long as this preamble remains intact and
;; as long as you state what changes you've made.

;; cdlatex.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fast input methods for LaTeX environments and math
;; Copyright (c) 1995, 1996, 1997, 2003 Carsten Dominik
;;
;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Keywords: tex
;; Version: 4.0
;;
;; This file is not part of GNU Emacs
;;
;; This program is free software you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.

;; The prefix command for all keys
(define-prefix-command 'latex-primer-prefix)

;; The function to enable it
(defun latex-primer-enable ()
  (local-set-key [(control \`)] 'latex-primer-prefix)
  ;customize TeX-electric-sub-and-superscript instead
  ;(local-set-key "^" 'cdlatex-sub-superscript)
  ;(local-set-key "_" 'cdlatex-sub-superscript)
  ;customize skeleton-pair instead
  (local-set-key "$" 'cdlatex-dollar)
  ;(local-set-key "(" 'cdlatex-pbb)
  ;(local-set-key "{" 'cdlatex-pbb)
  ;(local-set-key "[" 'cdlatex-pbb)
)

;; Add it to AUCTeX's mode hook
(defvar LaTeX-mode-hook nil)
(add-to-list 'LaTeX-mode-hook 'latex-primer-enable)

;; Begin math (ctrl+')
(define-key 'latex-primer-prefix [(control \')]
  (lambda () (interactive) (insert "$$") (backward-char)))

;; Begin math display mode (return)
(define-key 'latex-primer-prefix [(return)]
  (lambda () (interactive) (insert "\\[\\]") (backward-char 2)))

;; Begin math display mode on new line (ctrl+return)
(define-key 'latex-primer-prefix [(control return)]
  (lambda () (interactive)
    (LaTeX-environment-menu "equation*")
    ))

;; Vector arrow (asks for a variable name) (v)
(define-key 'latex-primer-prefix [(v)]
  (lambda (name) (interactive "cVector variable: ")
    (insert "\\vec{" name "}")))

;; Vector underline (asks for a variable name) (V)
(define-key 'latex-primer-prefix [(V)]
  (lambda (name) (interactive "cVector variable: ")
    (insert "\\b{" name "}")))

;; Square root (j)
(define-key 'latex-primer-prefix [(j)]
  (lambda () (interactive)
    (insert "\\sqrt{}") (backward-char)))

;; Indefinite integeral (asks for variable) (y)
(define-key 'latex-primer-prefix [(y)]
  (lambda (name) (interactive "cIntegral variable: ")
    (insert "\\int  \\,\\dd " name) (backward-char 8)))

;; Definite integral (asks for variable) (Y)
(define-key 'latex-primer-prefix [(Y)]
  (lambda (name) (interactive "cIntegral variable: ")
    (insert "\\int_{}^{}\\,\\dd " name " ") (backward-char 12)))

;; Fraction (/)
(define-key 'latex-primer-prefix [(/)]
  (lambda () (interactive) (insert "\\frac{}{}") (backward-char 3)))

;; Array (M)
(define-key 'latex-primer-prefix [(M)]
  (lambda () (interactive)
    (insert "\\begin{array}{rcl}\n"
            " & & \\\\\n"
            "\\end{array}")
    (backward-char 21)))

;; Cases (c)
(define-key 'latex-primer-prefix [(c)]
  (lambda () (interactive)
    (insert "\\begin{cases}\n"
            " & \\text{if $$,} \\\\\n"
            " & \\text{otherwise.}\n"
            "\\end{cases}")
    (backward-char 53)))

;; Simple bindings
(defmacro latex-primer-simple-key (key text)
  `(define-key 'latex-primer-prefix ,key
     (lambda () (interactive) (insert ,text))))

(latex-primer-simple-key "a" "\\alpha")
(latex-primer-simple-key "A" "\\forall")
;(latex-primer-simple-key "b" "\\beta") ; in use below
(latex-primer-simple-key "B" "\\mathbb{B}")
;(latex-primer-simple-key "c" "\\ ") ;; in use
(latex-primer-simple-key "C" "\\mathbb{C}")
;(latex-primer-simple-key "d" "\\delta")
;(latex-primer-simple-key "D" "\\Delta")
(latex-primer-simple-key "e" "\\varepsilon")
(latex-primer-simple-key "E" "\\exists")
;(latex-primer-simple-key "f" "\\phi") ; in use for mathfrak
;(latex-primer-simple-key "F" "\\varphi")
;(latex-primer-simple-key "g" "\\gamma")
;(latex-primer-simple-key "G" "\\Gamma")
(latex-primer-simple-key "h" "\\left|")
(latex-primer-simple-key "H" "\\right|")
;(latex-primer-simple-key "i" "\\iota") ; in use below
;(latex-primer-simple-key "I" "\\notin")
;(latex-primer-simple-key "j" "\\ ") ;; in use
(latex-primer-simple-key "J" "\\Join")
;(latex-primer-simple-key "k" "\\kappa")
(latex-primer-simple-key "K" "\\aleph")
;(latex-primer-simple-key "l" "\\lambda")
;(latex-primer-simple-key "L" "\\ell")
;(latex-primer-simple-key "m" "\\mu")
;(latex-primer-simple-key "M" "\\ ") ;; in use
(latex-primer-simple-key "n" "\\nabla")
(latex-primer-simple-key "N" "\\mathbb{N}")
;(latex-primer-simple-key "o" "\\Omega")
;(latex-primer-simple-key "O" "\\Theta")
;(latex-primer-simple-key "p" "\\pi") ;; in use below
(latex-primer-simple-key "P" "\\prod")
;(latex-primer-simple-key "q" "\\rhd")
(latex-primer-simple-key "Q" "\\mathbb{Q}")
;(latex-primer-simple-key "r" "\\rho")
(latex-primer-simple-key "R" "\\mathbb{R}")
(latex-primer-simple-key "s" "\\subseteq")
(latex-primer-simple-key "S" "\\subset")
;(latex-primer-simple-key "t" "\\theta")
;(latex-primer-simple-key "T" "\\tau")
(latex-primer-simple-key "u" "\\cup")
(latex-primer-simple-key "U" "\\cap")
;(latex-primer-simple-key "v" "\\ ") ;; in use
;(latex-primer-simple-key "V" "\\ ") ;; in use
;(latex-primer-simple-key "w" "\\leq")
;(latex-primer-simple-key "W" "\\geq")
(latex-primer-simple-key "x" "\\times")
(latex-primer-simple-key "X" "\\chi")
;(latex-primer-simple-key "y" "\\ ") ;; in use
;(latex-primer-simple-key "Y" "\\ ") ;; in use
(latex-primer-simple-key "z" "\\sigma")
(latex-primer-simple-key "Z" "\\mathbb{Z}")
(latex-primer-simple-key " " "\\quad")
(latex-primer-simple-key "_" "\\bot")
(latex-primer-simple-key "-" "\\vdash")
(latex-primer-simple-key "+" "\\sum")
(latex-primer-simple-key "=" "\\equiv")
(latex-primer-simple-key "~" "\\approx")
(latex-primer-simple-key "#" "\\neq")
(latex-primer-simple-key ":" "\\colon")
(latex-primer-simple-key ";" "\\Leftrightarrow")
(latex-primer-simple-key "*" "\\cdot")
(latex-primer-simple-key "&" "\\land")
(latex-primer-simple-key "?" "\\lor")
(latex-primer-simple-key "!" "\\neg")
(latex-primer-simple-key "%" "\\div")
(latex-primer-simple-key "\\" "\\setminus")
(latex-primer-simple-key "\"" "^")
(latex-primer-simple-key "'" "\\partial")
;(latex-primer-simple-key "(" "\\left(")
;(latex-primer-simple-key "[" "\\left[")
;(latex-primer-simple-key "{" "\\left\\{")
(latex-primer-simple-key ")" "\\right)")
(latex-primer-simple-key "]" "\\right]")
(latex-primer-simple-key "}" "\\right\\}")
(latex-primer-simple-key "<" "\\left\\langle")
(latex-primer-simple-key ">" "\\right\\rangle")
(latex-primer-simple-key "0" "\\emptyset")
;(latex-primer-simple-key "1" "\\mid") in use below
(latex-primer-simple-key "2" "\\to")
(latex-primer-simple-key "3" "\\ddots")
(latex-primer-simple-key "4" "\\Leftarrow")
(latex-primer-simple-key "5" "\\Rightarrow")
(latex-primer-simple-key "6" "\\Re")
(latex-primer-simple-key "7" "\\angle")
(latex-primer-simple-key "8" "\\infty")
(latex-primer-simple-key "9" "\\Im")

;; Custom definitions

;; Vector arrow (asks for a variable name) (v)
(define-key 'latex-primer-prefix [(b)]
  (lambda (name) (interactive "cmathbb variable: ")
    (insert "\\mathbb{" name "}")))

(define-key 'latex-primer-prefix [(f)]
  (lambda (name) (interactive "cmathfrak variable: ")
    (insert "\\mathfrak{" name "}")))

(define-key 'latex-primer-prefix [(I)]
  (lambda () (interactive) (insert "\\indicator_{}") (backward-char)))

;; Insert matched parentheses
(define-key 'latex-primer-prefix [(\()]
  (lambda () (interactive) (insert "\\left(\\right)") (backward-char 7)))
(define-key 'latex-primer-prefix [(\[)]
  (lambda () (interactive) (insert "\\left[\\right]") (backward-char 7)))
(define-key 'latex-primer-prefix [(\{)]
  (lambda () (interactive) (insert "\\left\\{\\right\\}") (backward-char 8)))

(latex-primer-simple-key "i" "\\iota")
(latex-primer-simple-key "p" "\\Prob")
(latex-primer-simple-key "," "\\vartriangleleft")
(latex-primer-simple-key "." "\\vartriangleright")
(latex-primer-simple-key "l" "\\ell")


(defconst cdlatex-parens-pairs '(("(".")") ("["."]") ("{"."}")
                               ("|"."|") ("<".">")))

(defun cdlatex-pbb ()
  "Insert a pair of parens, brackets or braces."
  (interactive)
  (let ((paren (char-to-string last-command-char)))
    (if (and (stringp cdlatex-paired-parens)
             (string-match (regexp-quote paren) cdlatex-paired-parens)
             (not (cdlatex-number-of-backslashes-is-odd)))
        (progn
          (insert paren)
          (insert (cdr (assoc paren cdlatex-parens-pairs)))
          (forward-char -1))
      (insert paren))))

(defun cdlatex-dollar (&optional arg)
  "Insert a pair of dollars unless number of backslashes before point is odd.
With arg, insert pair of double dollars."
  (interactive "P")
  (if (cdlatex-number-of-backslashes-is-odd)
      (insert "$")
    (if (texmathp)
        (if (and (stringp (car texmathp-why))
                 (equal (substring (car texmathp-why) 0 1) "\$"))
            (progn
              (insert (car texmathp-why))
              (save-excursion
                (goto-char (cdr texmathp-why))
                (if (pos-visible-in-window-p)
                    (sit-for 1))))
          (message "No dollars inside a math environment!")
          (ding))
      (if (and (stringp cdlatex-paired-parens)
               (string-match "\\$" cdlatex-paired-parens))
          (if arg
              (if (bolp)
                  (progn (insert "\$\$\n\n\$\$\n") (backward-char 4))
                (insert "\$\$  \$\$") (backward-char 3))
            (insert "$$") (backward-char 1))
        (if arg
            (if (bolp) (insert "$$\n") (insert "$$"))
          (insert "$"))))))

(defvar cdlatex-paired-parens "$[{(")



;;    The keys `_' and `^' will insert "_{}" and "^{}", respectively,
;;    and, if necessary, also a pair of dollar signs to switch to math
;;    mode.  You can use TAB to exit paired parenthesis.  As a special
;;    case, when you use TAB to exit a pair of braces that belong to a
;;    subscript or superscript, CDLaTeX removes the braces if the
;;    sub/superscript consists of a single character.  For example
;;    typing "$10^3<TAB>" inserts "$10^3$", but typing "$10^34<TAB>"
;;    inserts "$10^{34}$"

;; This is not needed anymore, just customize TeX-electric-sub-and-superscript
(defun cdlatex-sub-superscript ()
  "Insert ^{} or _{} unless the number of backslashes before point is odd.
When not in LaTeX math environment, _{} and ^{} will have dollars."
  (interactive)
  (if (cdlatex-number-of-backslashes-is-odd)
      ;; Quoted
      (insert last-command-char)
    ;; Check if we need to switch to math mode
    (if (not (texmathp)) (cdlatex-dollar))
    (if (string= (buffer-substring (max (point-min) (- (point) 2)) (point))
                 (concat (char-to-string last-command-char) "{"))
        ;; We are at the start of a sub/suberscript.  Allow a__{b} and a^^{b}
        ;; This is an undocumented feature, please keep it in.  It supports
        ;; a special notation which can be used for upright sub- and
        ;; superscripts.
        (progn
          (backward-char 1)
          (insert last-command-char)
          (forward-char 1))
      ;; Insert the normal template.
      (insert last-command-char)
      (insert "{}")
      (forward-char -1))))

(defun cdlatex-number-of-backslashes-is-odd ()
  ;; Count backslashes before point and return t if number is odd.
  (let ((odd nil))
    (save-excursion
      (while (equal (preceding-char) ?\\)
        (progn
          (forward-char -1)
          (setq odd (not odd)))))
    (setq odd odd)))
