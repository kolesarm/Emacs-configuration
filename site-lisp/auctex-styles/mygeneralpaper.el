;; Tell reftex to use natbib-style citations if I load mygeneral
(TeX-add-style-hook "mygeneralpaper"
   (lambda ()
     (TeX-run-style-hooks "natbib")))
