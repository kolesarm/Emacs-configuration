;;; ob-stata.el --- org-babel functions for stata code evaluation
;; Dowloaded from
;; https://gitlab.com/robhicks/ob-stata.el/blob/master/ob-stata.el. Rob Hicks
;; modified the version at
;; https://github.com/aspiers/orgmode/blob/master/contrib/lisp/ob-stata.el

;; Copyright (C) 2014 Ista Zahn
;; Author: Ista Zahn istazahn@gmail.com
;;      G. Jay Kerns
;;      Eric Schulte
;;      Dan Davison
;;      Rob Hicks

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The file provides Org-Babel support for evaluating stata code.
;; It is basically result of find-and-replace "stata" for "julia"
;; in ob-julia.el by G. Jay Kerns. Only ":results output" works: the
;; header args must include ":results output" (this is the default).
;; Note that I'm not sure ':results value' makes sense or is useful
;; but I have left all the value-processing stuff inherited from
;; ob-julia and ob-R. ':results graphics' would be nice, but I have
;; not tried to implement it.
;; --Ista, 07/30/2014

;; The version is changed from Ista's in 4 ways:
;; 1. Some API changes necessitated changes as described near
;; the bottom of this comment:
;; https://emacs.stackexchange.com/questions/29885/error-when-stata-t-added-to-org-babel-do-load-languages-in-an-attempt-to-eva/39099
;; 2. The "Ista 2014" version returns results and commands, so
;; :exports both will have fontified commands and plain text
;; commands in the output.  This version removes commands from
;; the output for most cases.
;; 3. The regex to clean up extra prompts was overly aggressive and
;; messed with some output like bstrap and estat classification. It has been
;; toned down and I'm not sure it is even necessary.
;; 4. Line continuation (using ///) is supported
;; --Rob, 04/13/2018

;;; Requirements:
;; Stata: http://stata.com
;; ESS: http://ess.r-project.org

;;; Code:
(require 'ob)
(require 'cl-lib)

(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function stata "ext:ess-stata" (&optional start-args))
(declare-function inferior-ess-send-input "ext:ess-inf" ())
(declare-function ess-make-buffer-current "ext:ess-inf" ())
(declare-function ess-eval-buffer "ext:ess-inf" (vis))
(declare-function org-number-sequence "org-compat" (from &optional to inc))

(defconst org-babel-header-args:stata
  '((width		 . :any)
    (horizontal		 . :any)
    (results             . ((file list vector table scalar verbatim)
			    (raw org html latex code pp wrap)
			    (replace silent append prepend)
                            ;; NOTE: not sure 'value' makes sense in stata
                            ;; we may want to remove it from the list
			    (output value graphics))))
  "stata-specific header arguments.")

(add-to-list 'org-babel-tangle-lang-exts '("stata" . "do"))

;; only ':results output' currently works, so make that the default
(defvar org-babel-default-header-args:stata '((:results . "output")))

(defcustom org-babel-stata-command inferior-STA-program-name
  "Name of command to use for executing stata code."
  :group 'org-babel
  :version "25.1"
  :package-version '(Org . "8.3")
  :type 'string)

(defvar ess-local-process-name) ; dynamically scoped
(defun org-babel-edit-prep:stata (info)
  (let ((session (cdr (assq :session (nth 2 info)))))
    (when (and session (string-match "^\\*\\(.+?\\)\\*$" session))
      (save-match-data (org-babel-stata-initiate-session session nil)))))

(defun org-babel-expand-body:stata (body params &optional graphics-file)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((graphics-file
	 (or graphics-file (org-babel-stata-graphical-output-file params))))
    (mapconcat
     #'identity
     ((lambda (inside)
	(if graphics-file
            inside
	  inside))
      (append (org-babel-variable-assignments:stata params)
	      (list body))) "\n")))

(defun org-babel-execute:stata (body params)
  "Execute a block of stata code.
This function is called by `org-babel-execute-src-block'."
  (save-excursion
    (let* ((result-params (cdr (assq :result-params params)))
	   (result-type (cdr (assq :result-type params)))
           (session (org-babel-stata-initiate-session
		     (cdr (assq :session params)) params))
	   (colnames-p (cdr (assq :colnames params)))
	   (rownames-p (cdr (assq :rownames params)))
	   (graphics-file (org-babel-stata-graphical-output-file params))
	   (full-body (org-babel-expand-body:stata body params graphics-file))
	   (result
	    (org-babel-stata-evaluate
	     session full-body result-type result-params
	     (or (equal "yes" colnames-p)
		 (org-babel-pick-name
		  (cdr (assq :colname-names params)) colnames-p))
	     (or (equal "yes" rownames-p)
		 (org-babel-pick-name
		  (cdr (assq :rowname-names params)) rownames-p)))))
      (if graphics-file nil result))))

(defun org-babel-prep-session:stata (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-stata-initiate-session session params))
	 (var-lines (org-babel-variable-assignments:stata params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-load-session:stata (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:stata session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer)))) 
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:stata (params)
  "Return list of stata statements assigning the block's variables."
  (let ((vars (mapcar #'cdr (org-babel--get-vars params))))
    (mapcar
     (lambda (pair)
       (org-babel-stata-assign-elisp
	(car pair) (cdr pair)
	(equal "yes" (cdr (assq :colnames params)))
	(equal "yes" (cdr (assq :rownames params)))))
     (mapcar
      (lambda (i)
	(cons (car (nth i vars))
	      (org-babel-reassemble-table
	       (cdr (nth i vars))
	       (cdr (nth i (cdr (assq :colname-names params))))
	       (cdr (nth i (cdr (assq :rowname-names params)))))))
      (org-number-sequence 0 (1- (length vars)))))))

(defun org-babel-stata-quote-csv-field (s)
  "Quote field S for export to stata."
  (if (stringp s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    (format "%S" s)))

(defun org-babel-stata-assign-elisp (name value colnames-p rownames-p)
  "Construct stata code assigning the elisp VALUE to a variable named NAME."
  (if (listp value)
      (let ((max (apply #'max (mapcar #'length (cl-remove-if-not
						#'sequencep value))))
	    (min (apply #'min (mapcar #'length (cl-remove-if-not
						#'sequencep value))))
	    (transition-file (org-babel-temp-file "stata-import-")))
        ;; ensure VALUE has an orgtbl structure (depth of at least 2)
        (unless (listp (car value)) (setq value (list value)))
        (with-temp-file transition-file
          (insert
	   (orgtbl-to-csv value '(:fmt org-babel-stata-quote-csv-field))
	   "\n"))
	(let ((file (org-babel-process-file-name transition-file 'noquote))
	      (header (if (or (eq (nth 1 value) 'hline) colnames-p)
			  "TRUE" "FALSE"))
	      (row-names (if rownames-p "1" "NULL")))
	  (if (= max min)
	      (format "%s = insheet using \"%s\"" name file)
	    (format "%s = insheet using \"%s\""
		    name file))))
    (format "%s = %s" name (org-babel-stata-quote-csv-field value))))

(defvar ess-ask-for-ess-directory) ; dynamically scoped

(defun org-babel-stata-initiate-session (session params)
  "If there is not a current stata process then create one."
  (unless (string= session "none")
    (let ((session (or session "*stata*"))
	  (ess-ask-for-ess-directory
	   (and (and (boundp 'ess-ask-for-ess-directory) ess-ask-for-ess-directory)
		(not (cdr (assq :dir params))))))
      (if (org-babel-comint-buffer-livep session)
	  session
	(save-window-excursion
	  (require 'ess) (stata)
	  (rename-buffer
	   (if (bufferp session)
	       (buffer-name session)
	     (if (stringp session)
		 session
	       (buffer-name))))
	  (current-buffer))))))

(defun org-babel-stata-associate-session (session)
  "Associate stata code buffer with a stata session.
Make SESSION be the inferior ESS process associated with the
current code buffer."
  (setq ess-local-process-name
	(process-name (get-buffer-process session)))
  (ess-make-buffer-current))

(defun org-babel-stata-graphical-output-file (params)
  "Name of file to which stata should send graphical output."
  (and (member "graphics" (cdr (assq :result-params params)))
       (cdr (assq :file params))))

(defvar org-babel-stata-eoe-indicator "display \"org_babel_stata_eoe\"")
(defvar org-babel-stata-eoe-output "org_babel_stata_eoe")

(defvar org-babel-stata-write-object-command "outsheet using \"%s\"")

(defun org-babel-stata-strip-commands (result body)
  "Takes two text strings and eliminates duplicates from results
   (contains commands and results) that are in body (contains
   commands), and returns a scrubbed version of results. This
   only works in session mode and for :results output"
  (mapconcat 'identity (set-difference 
			(mapcar 'string-trim-right (s-split "\\\n" result)) 
			(mapcar 'string-trim-right (s-split "\\\n" body)) 
			:test #'equal)"\n")) 

(defun org-babel-stata-evaluate
    (session body result-type result-params column-names-p row-names-p)
  "Evaluate stata code in BODY after line continuation syntax stripped."
  (if session
      (progn
	(setq body_stripped body)
	(setq body_stripped (replace-regexp-in-string "\/\/\/\n" "" body_stripped))
	(org-babel-stata-evaluate-session
	 session body_stripped result-type result-params column-names-p row-names-p))
    (org-babel-stata-evaluate-external-process
     body result-type result-params column-names-p row-names-p)))

(defun org-babel-stata-evaluate-external-process
    (body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in external stata process.
If RESULT-TYPE equals 'output then return standard output as a
string.  If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (cl-case result-type
    (value
     (let ((tmp-file (org-babel-temp-file "stata-")))
       (org-babel-eval org-babel-stata-command
		       (format org-babel-stata-write-object-command
			       (org-babel-process-file-name tmp-file 'noquote)
			       (format "begin\n%s\nend" body)))
       (org-babel-stata-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (buffer-string))
	  (org-babel-import-elisp-from-file tmp-file '(4)))
	column-names-p)))
    (output (org-babel-eval org-babel-stata-command body))))

(defun org-babel-stata-evaluate-session
    (session body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in SESSION.
If RESULT-TYPE equals 'output then return standard output as a
string.  If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (cl-case result-type
    (value
     (with-temp-buffer
       (insert (org-babel-chomp body))
       (let ((ess-local-process-name
	      (process-name (get-buffer-process session)))
	     (ess-eval-visibly-p nil))
	 (ess-eval-buffer nil)))
     (let ((tmp-file (org-babel-temp-file "stata-")))
       (org-babel-comint-eval-invisibly-and-wait-for-file
	session tmp-file
	(format org-babel-stata-write-object-command
		(org-babel-process-file-name tmp-file 'noquote) "ans"))
       (org-babel-stata-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (buffer-string))
	  (org-babel-import-elisp-from-file tmp-file '(4)))
	column-names-p)))
    (output
     (org-babel-stata-strip-commands
      (mapconcat
       #'org-babel-chomp
       (butlast
	(delq nil
	      (mapcar
	       (lambda (line) (when (> (length line) 0) line))
	       (mapcar
		(lambda (line) ;; cleanup extra prompts left in output
		  (if (string-match
		       "^[.:]" line)
		      (substring line (match-end 1))
		    line))
		(org-babel-comint-with-output (session org-babel-stata-eoe-output)
		  (insert (mapconcat #'org-babel-chomp
				     (list body org-babel-stata-eoe-indicator)
				     "\n"))
		  (inferior-ess-send-input)))))) "\n") body))))

(defun org-babel-stata-process-value-result (result column-names-p)
  "stata-specific processing of return value.
Insert hline if column names in output have been requested."
  (if column-names-p
      (cons (car result) (cons 'hline (cdr result)))
    result))

(provide 'ob-stata)

;;; ob-stata.el ends here
