;; Tell reftex to use natbib-style citations if I load mygeneral
(TeX-add-style-hook "mygeneralpaper"
   (lambda ()
     (if (fboundp 'reftex-set-cite-format)
         (reftex-set-cite-format 'natbib))))
