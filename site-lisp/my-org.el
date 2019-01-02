;; Time-stamp: <2019-01-02 12:18:33 (kolesarm)>

;;; 1. ORG-MODE

;; .org files use org-mode by default since Emacs 22.2,
;; Use M-+ and M-- instead of S-<arrow> to toggle:
;; (setq org-replace-disputed-keys t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-ellipsis "⤵")

;; default is t, since tables look terrible. I'd rather not have long lines
;; cut-off by default, however
(setq org-startup-truncated nil)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)
;; TAB in a code block is as if it were issued in the language major mode
(setq org-src-tab-acts-natively t)

;; Only needed when a capture template doesn't use absolute path, and when
;; capture note is filed away interactively
(setq org-directory "~/manuals/")

;; Enable stata, sh and R code evaluation, disable lisp
;; As of Emacs 26.1 need shell rather than sh, which points to /bin/sh specifically
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil) (R . t) (shell . t)  (stata . t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil) (R . t) (stata . t)))

;; A list of packages to be inserted in every LaTeX header. I'd rather do it from
;; .org file
;; (add-to-list 'org-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-latex-packages-alist '("" "color"))

; org-babel. By default, only emacs-list is enabled
(org-babel-do-load-languages
  'org-babel-load-languages
    '((R . t)
      (shell . t)
      (latex . t)
      (matlab . t)
      (python . t)
      (emacs-lisp . t)
      (org . t)
      ))

;;As of release 8.0, Org-mode has transitioned to a new export framework. Some
;;backends have to be loaded explicitly.
(add-hook 'org-mode-hook
          (lambda()
            (require 'ox-md)
            ;(require 'ox-odt)
            (require 'ob-stata)      ; Org-Babel support for stata
))


;; Colored export of source code
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(setq org-export-latex-listings t)

;; reftex overwrites this
(add-hook 'org-mode-hook
  (lambda ()
    (define-key org-mode-map "\C-c+" 'org-ctrl-c-minus)))


;; Open .pdf files with evince
(eval-after-load "org"
  '(progn
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))



;;; 2. TODO.TXT
;; there are two modes, this first one sucks
;; (require 'todotxt)
(require 'todotxt-mode)
(add-to-list 'auto-mode-alist '("\\todo.txt$" . todotxt-mode))
(setq todotxt-default-file
      (expand-file-name "/home/kolesarm/Dropbox/todo/todo.txt"))

(provide 'my-org)
;;; my-org.el ends here
