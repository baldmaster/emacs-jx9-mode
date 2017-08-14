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

;; Indentation level
(defcustom jx9-tab-width 2
  "Number of spaces to indent nested statements."
  :group 'jx9
  :safe 'integerp
  :type 'integer)

;; Keymap
(defvar jx9-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Jx9 major mode")

;; Custom faces

(defface jx9-nowdoc-face
  '((t :foreground "medium orchid"
       :weight bold
       ))
  "Face for nowdoc strings."
  :group 'jx9-mode )

(defface jx9-function-call-face
  '((t :foreground "cyan"))
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

(defconst jx9-builtin-consts-re
  (regexp-opt
   '(
     "JX9_VERSION"
     "JX9_ENGINE"
     "__JX9__"
     "JX9_OS"
     "__OS__"
     "JX9_INT_MAX"
     "JX9_EOL"
     "MAXINT"
     "JX9_INT_SIZE"
     "PATH_SEPARATOR"
     "DIRECTORY_SEPARATOR"
     "DIR_SEP"
     "__TIME__"
     "__DATE__"
     "__FILE__"
     "__DIR__"
     "COUNT_NORMAL"
     "COUNT_RECURSIVE"
     "SORT_ASC"
     "SORT_DESC"
     "SORT_REGULAR"
     "SORT_NUMERIC"
     "SORT_STRING"
     "JX9_URL_SCHEME"
     "JX9_URL_HOST"
     "JX9_URL_PORT"
     "JX9_URL_USER"
     "JX9_URL_PASS"
     "JX9_URL_PATH"
     "JX9_URL_QUERY"
     "JX9_URL_FRAGMENT"
     "JX9_QUERY_RFC1738"
     "JX9_QUERY_RFC3986"
     "SEEK_SET"
     "SEEK_CUR"
     "SEEK_END"
     "LOCK_EX"
     "LOCK_SH"
     "LOCK_NB"
     "LOCK_UN"
     "STDIN"
     "stdin"
     "STDOUT"
     "stdout"
     "STDERR"
     "stderr"
     "SCANDIR_SORT_ASC"
     "SCANDIR_SORT_DESC"
     "SCANDIR_SORT_NONE"
     "GLOB_MARK"
     "GLOB_NOSORT"
     "GLOB_NOCHECK"
     "GLOB_NOESCAPE"
     "GLOB_BRACE"
     "GLOB_ONLYDIR"
     "GLOB_ERR"
     "FILE_USE_INCL_PATH"
     "FILE_IGN_NL"
     "FILE_SKIP_EL"
     "FILE_APPEND") t))

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

(defconst jx9-function-declaration-re
  (concat
   "\\s-*function\\("
   jx9-identifier-re
   "\\)\\s-*("))

(defconst jx9-function-call-re
  (concat
   "\\s-*\\("
   jx9-identifier-re
   "\\)\\s-*(.*)"))

;; Font lock keywords
(setq jx9-font-lock-keywords
      `((,jx9-nowdoc-re . 'jx9-nowdoc-face)
        (,jx9-keywords-re . font-lock-keyword-face)
        (,jx9-builtin-consts-re . font-lock-keyword-face)
        (,jx9-function-declaration-re (1 font-lock-string-face))
        (,jx9-function-call-re (1 'jx9-function-call-face))
        (,jx9-variable-assignment (2 font-lock-variable-name-face))
        (,jx9-for-variable-re  (1 font-lock-variable-name-face))))

;; Syntax table
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

(defun jx9-indent-line ()
  "Indent current line."
  (interactive)
  (beginning-of-line)
  (if (bobp)
	  (indent-line-to 0)
	(let ((not-indented t) cur-indent)
	  (if (looking-at "^[ \t]*}")
		  (progn
			(save-excursion
			  (forward-line -1)
			  (setq cur-indent (- (current-indentation) jx9-tab-width)))
			(if (< cur-indent 0)
				(setq cur-indent 0)))
		(save-excursion
		  (while not-indented
			(forward-line -1)
			(if (looking-at "^[ \t]*}")
				(progn
				  (setq cur-indent (current-indentation))
				  (setq not-indented nil))
			  (if (looking-at ".*[ \t]*{")
				  (progn
					(setq cur-indent (+ (current-indentation) jx9-tab-width))
					(setq not-indented nil))
				(if (bobp)
					(setq not-indented nil)))))))
	  (if cur-indent
		  (indent-line-to cur-indent)
		(indent-line-to 0)))))

;; Mode
(defun jx9-mode ()
  "Major mode for editing Jx9"
  (interactive)
  (kill-all-local-variables)
  (setq-local font-lock-multiline t)
  (set-syntax-table jx9-mode-syntax-table)
  (use-local-map jx9-mode-map)
  (set (make-local-variable 'indent-line-function) 'jx9-indent-line)
  (set (make-local-variable 'font-lock-defaults) '(jx9-font-lock-keywords))
  (add-hook 'font-lock-extend-region-functions 'jx9-font-lock-extend-region)
  (setq major-mode 'jx9-mode)
  (setq mode-name "Jx9"))

;;;###autoload
(add-to-list 'auto-mode-alist '(".\\jx9$'" . jx9-mode))

(provide 'jx9-mode)
;;; jx9-mode.el ends here
