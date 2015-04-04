;; Tell reftex to use natbib-style citations if I load mygeneralnotes
(TeX-add-style-hook "mygeneralnotes"
   (lambda ()
     (TeX-run-style-hooks "natbib")))
