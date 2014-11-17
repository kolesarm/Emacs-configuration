;;; from listings.el
(TeX-add-style-hook
 "mymanuals"
 (lambda ()
   ;; New symbols
   (TeX-add-symbols
    '("lstalias" ["Alias dialect"] "Alias" ["Dialect"] "Language")
    '("lstdefinestyle" "Style name" "Arguments (key=value list)")
    '("lstinline" TeX-arg-verb)
    '("lstinputlisting" ["Arguments (key=value list)"] TeX-arg-file)
    "lstlistoflistings"
    '("lstnewenvironment" "Name" ["Number or arguments"] ["Default argument"]
      "Starting code" "Ending code")
    "lstset")
   ;; New environments
   (LaTeX-add-environments
    "lstlisting")
   ;; Filling
   (make-local-variable 'LaTeX-indent-environment-list)
   (add-to-list 'LaTeX-indent-environment-list
                '("lstlisting" current-indentation))
   (make-local-variable 'LaTeX-verbatim-regexp)
   (setq LaTeX-verbatim-regexp (concat LaTeX-verbatim-regexp "\\|lstlisting"))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("lstnewenvironment" "{[[{{")) 'function)
     (font-latex-add-keywords '(("lstinputlisting" "[{")) 'reference)
     (font-latex-add-keywords '(("lstinline" "[{") ; The second argument should
                                                   ; actually be verbatim.
                                ("lstlistoflistings" ""))
                              'textual)
     (font-latex-add-keywords '(("lstalias" "{{")
                                ("lstdefinestyle" "{{")
                                ("lstset" "{"))
                              'variable)
     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults))))

;;; listings.el ends here
