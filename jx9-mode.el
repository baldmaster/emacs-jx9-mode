;;; jx9-mode.el --- Major mode for jx9 files
;;
;; Copyright (c) 2017 Sergey Skupoy
;;
;; Author: Sergey Skupoy
;;
;; Maintainer: Sergey Skupoy <sergey.skupoy@gmail.com>
;; Created: August 11, 2017
;;
;; Version: 0.0.1
;; Homepage: https://github.com/baldmaster/emacs-jx9-mode
;; Keywords: language, jx9
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `jx9-mode' offers Emacs support for Jx9 language (http://jx9.symisc.net/)
;;
;;; Code:

(defconst jx9-keywords-re
  (regexp-opt
   '("break"
     "if"
     "else"
     "elseif"
     "for"
     "foreach"
     "while"
     "do"
     "switch"
     "static"
     "function"
     "case"
     "print"
     "const"
     "default"
     "as"
     "continue"
     "exit"
     "die"
     "import"
     "include"
     "string"
     "bool"
     "boolean"
     "int"
     "integer"
     "float"
     "uplink"
     "class"
     "object"
     "array"
     "return"
     "goto") t))

(defconst jx9-variable-re
  "\\($[^[:digit:]][\\([:alnum:]|_\\)]*\\)")

(defconst jx9-for-variable-re
  (concat
   "^\\s-*for\\s-*(\\s-*"
   jx9-variable-re))

(defconst jx9-variable-assignment
  (concat
   "^\\s-*\\(static\\s-+\\)?"
   jx9-variable-re
   "\\s-*="))


(setq jx9-font-lock-keywords
      `((,jx9-keywords-re . font-lock-keyword-face)
        (,jx9-variable-assignment .  font-lock-variable-name-face)
        (,jx9-for-variable-re  (1 font-lock-variable-name-face))))


(defvar jx9-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defun jx9-mode ()
  "Major mode for editing Jx9"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table jx9-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(jx9-font-lock-keywords))
  (setq major-mode 'jx9-mode)
  (setq mode-name "Jx9"))

;;;###autoload
(add-to-list 'auto-mode-alist '(".\\jx9$'" . jx9-mode))

(provide 'jx9-mode)
;;; jx9-mode.el ends here
