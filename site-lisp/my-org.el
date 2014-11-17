;; Time-stamp: <2014-11-14 10:36:18 (kolesarm)>

;;;; ORG-MODE
; automatically in Emacs 22
;(require 'org-latex) ;make latex work!
; org-beamer --- for some reason it's not part of my emacs build
;(require 'org-beamer)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M--
;; instead to toggle) I left S-<arrow> on
;; (setq org-replace-disputed-keys t)


(setq org-directory "~/manuals/")

; make code colorful
(setq org-export-latex-listings t)
;; used to be called org-export-latex-packages-alist. A cell is of the format
;; ("options" "package" SNIPPET-FLAG)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

(add-hook 'org-mode-hook 'turn-on-reftex) ; reftex
;(add-hook 'org-mode-hook 'turn-on-org-cdlatex) ;latex shortcuts don't want cdlatex, perhaps? Else I need to download it, I think.
(add-hook 'org-mode-hook
  (lambda ()
    (define-key org-mode-map "\C-c+" 'org-ctrl-c-minus))) ;reftex overwrites this

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

; org-babel
(org-babel-do-load-languages
  'org-babel-load-languages
    '((R . t)
      (sh . t)
      ))

;;;; TODO.TXT
;; there are two modes, this first one sucks
;; (require 'todotxt)
(require 'todotxt-mode)
(add-to-list 'auto-mode-alist '("\\todo.txt$" . todotxt-mode))
(setq todotxt-default-file
      (expand-file-name "/home/kolesarm/Dropbox/todo/todo.txt"))
;; (define-key global-map "\C-co" 'todotxt-open-file)
;; (define-key global-map "\C-ct" 'todotxt-add-todo)

;; Templates
; <n adds comment
(add-to-list 'org-structure-template-alist
             '("n" "#+BEGIN_COMMENT\n?\n#+END_COMMENT"
               "<comment>\n?\n</comment>"))

;; MobileOrg
;; (setq org-mobile-directory "~/docs/Dropbox/org")
;; (setq org-mobile-files '("~/manuals/cooking/recipes.org"))
;; (setq org-mobile-inbox-for-pull "~/manuals/from-mobile.org")


;(setq org-agenda-files
;      (list "~/org/research.org" "~/org/teaching.org" "~/org/work.org" "~/org/personal.org"))

; remember---I don't use it!
;(add-to-list 'load-path "~/.emacs.d/remember")
;(require 'remember)

;(org-remember-insinuate)

;(setq org-default-notes-file (concat org-directory "/work.org"))
;(setq remember-annotation-functions '(org-remember-annotation))
;(setq remember-handler-functions '(org-remember-handler))
;(add-hook 'remember-mode-hook 'org-remember-apply-template)

;(define-key global-map "\C-cr" 'org-remember)
;(setq org-remember-templates
;      '(("Todo" ?t "* TODO %^{Description}\n%?\nAdded: %U\nDEADLINE: %^T" "~/org/work.org" "Tasks")
;        ("Meeting" ?m "\n** %^{Who} \n SCHEDULED: %^T %? \n %^{LOCATION}p%i" "~/org/work.org" "Meetings")
;        ("Seminar" ?s "\n** %^{Who} \n SCHEDULED: %^T %? \n %^{LOCATION}p%i" "~/org/work.org" "Seminars")
;        ("Idea" ?i "* %^{Title} %^g\n %? %i\n %a" "~/org/personal.org" "Ideas")
;        ("Contact" ?c "\n** %^{Name}\n %[~/org/contact.txt]" "~/org/personal.org" "Contacts")
;        ))
; %a: annotation %i: initial content %t timestamp, date only %T time also
; ? cursor, %^T prompt for date %^{PROPERTY}p prompt for property




(provide 'my-org)
