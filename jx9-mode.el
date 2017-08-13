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


;; Custom faces


(defface jx9-nowdoc-face
  '((t :foreground "medium orchid"
       :weight bold
       ))
  "Face for nowdoc strings."
  :group 'jx9-mode )

;; Regexps
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

(defconst jx9-identifier-re
  "[^[:digit:]][\\([:alnum:]|_\\)]*")

(defconst jx9-variable-re
  (concat
   "\\($"
   jx9-identifier-re
   "\\)"))

(defconst jx9-for-variable-re
  (concat
   "^\\s-*for\\s-*(\\s-*"
   jx9-variable-re))

(defconst jx9-variable-assignment
  (concat
   "^\\s-*\\(static\\s-+\\)?"
   jx9-variable-re
   "\\s-*="))

(defconst jx9-nowdoc-start-re
  (concat
   "\\s-?<<<\\("
   jx9-identifier-re
   "\\)\\s-*\n"))

(defconst jx9-nowdoc-re
  (concat
   jx9-nowdoc-start-re "\\(.\\|\n\\)*?\n\\1;"))

(setq jx9-font-lock-keywords
      `((,jx9-nowdoc-re . 'jx9-nowdoc-face)
        (,jx9-keywords-re . font-lock-keyword-face)
        (,jx9-variable-assignment (2 font-lock-variable-name-face))
        (,jx9-for-variable-re  (1 font-lock-variable-name-face))))

(defvar jx9-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defun jx9-font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  ;; TODO: improve efficiency.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "\n\n" nil t)
        (beginning-of-line)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))

(defun jx9-mode ()
  "Major mode for editing Jx9"
  (interactive)
  (kill-all-local-variables)
  (setq-local font-lock-multiline t)
  (set-syntax-table jx9-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(jx9-font-lock-keywords))
  (add-hook 'font-lock-extend-region-functions 'jx9-font-lock-extend-region)
  (setq major-mode 'jx9-mode)
  (setq mode-name "Jx9"))

;;;###autoload
(add-to-list 'auto-mode-alist '(".\\jx9$'" . jx9-mode))

(provide 'jx9-mode)
;;; jx9-mode.el ends here
