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

(defconst jx9-keywords-regexp
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

(defconst jx9-variable-regexp
  "^.*\\s-+\\($[^[:digit:]][\\([:alnum:]|_\\)]*\\)")

(setq jx9-font-lock-keywords
      `((,jx9-keywords-regexp . font-lock-keyword-face)
        (,jx9-variable-regexp . (1 font-lock-variable-name-face))))

(defun jx9-mode ()
  "Major mode for editing Jx9"
  (interactive)
  (kill-all-local-variables)
  (setq font-lock-defaults '((jx9-font-lock-keywords)))
  (setq major-mode         'jx9-mode)
  (setq mode-name "Jx9"))

;;;###autoload
(add-to-list 'auto-mode-alist '(".\\jx9$'" . jx9-mode))

(provide 'jx9-mode)
;;; jx9-mode.el ends here
