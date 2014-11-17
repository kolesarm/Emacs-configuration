;; Tell reftex to use eqref if I load mymathdn
(TeX-add-style-hook
 "mymathdn"
 (lambda ()
       (add-to-list 'reftex-label-alist 'AMSTeX)))

;; (TeX-add-style-hook
;;  "mymathdn"
;;  (lambda ()
;;      ;; (if (fboundp 'reftex-add-label-environments)
;;      ;;     (reftex-add-label-environments '(AMSTeX))))
;;    (setq reftex-label-alist '(AMSTeX))
;;    ))


;; See and add all custom environments
;; http://www.gnu.org/s/auctex/manual/reftex/Style-Files.html#Style-Files
