;; From MELPA
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Available R linters:
;; - Syntax errors :: reported by parse.
;; - object_usage_linter :: checks that closures have the proper usage using
;;      codetools::checkUsage(). Note this runs base::eval() on the code, so do not
;;      use with untrusted code.
;; - absolute_paths_linter :: checks that no absolute paths are used.
;; - assignment_linter :: checks that <- is always used for assignment
;; - closed_curly_linter :: check that closed curly braces should always be on
;;      their own line unless they follow an else.
;; - commas_linter :: check that all commas are followed by spaces, but do not have
;;                    spaces before them.
;; - infix_spaces_linter :: check that all infix operators have spaces around them.
;; - line_length_linter :: check the line length of both comments and code is less
;;      than length.
;; - no_tab_linter :: check that only spaces are used, never tabs.
;; - camel_case_linter :: check that function and variable names are not camelCase.
;; - snake_case_linter :: check that function and variable names are not
;;      snake_case.
;; - multiple_dots_linter :: check that function and variable names are separated
;;      by _ rather than ..
;; - object_length_linter :: check that function and variable names are not more
;;      than length characters.
;; - open_curly_linter :: check that opening curly braces are never on their own
;;      line and are always followed by a newline.
;; - single_quotes_linter :: checks that only single quotes are used to delimit
;;      string contestants.
;; - spaces_inside_linter :: check that parentheses and square brackets do not have
;;      spaces directly inside them.
;; - spaces_left_parentheses_linter :: check that all left parentheses have a space
;;      before them unless they are in a function call.
;; - trailing_blank_lines_linter :: check there are no trailing blank lines.
;; - trailing_whitespace_linter :: check there are no trailing whitespace characters.

;; Make sure package("lintr") is installed when upgrading R
(customize-set-variable 'flycheck-lintr-linters
                        "with_defaults(line_length_linter(80), commented_code_linter=NULL, infix_spaces_linter=NULL, camel_case_linter=NULL, spaces_left_parentheses_linter=NULL)")


(provide 'my-init-flycheck)

;;; my-init-flycheck.el ends here
