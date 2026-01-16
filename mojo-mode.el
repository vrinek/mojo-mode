;;; mojo-mode.el --- Major mode for editing Mojo source code -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kostas

;; Author: Kostas <vrinek@hey.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, mojo
;; URL: https://github.com/vrinek/mojo-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; A major mode for editing Mojo programming language source files.

;;; Code:

(defvar mojo-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    table)
  "Syntax table for `mojo-mode'.")

(defvar mojo-font-lock-keywords
  `((,(regexp-opt '("def" "fn" "struct" "trait" "alias"
                    "var"
                    "if" "elif" "else" "for" "while"
                    "break" "continue" "return" "pass"
                    "raise" "try" "except" "finally"
                    "with" "as" "match" "case"
                    "import" "from"
                    "and" "or" "not" "in" "is"
                    "lambda" "yield" "async" "await"
                    "raises" "owned" "borrowed" "inout" "read")
                  'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt '("Int" "UInt" "Int8" "Int16" "Int32" "Int64"
                    "UInt8" "UInt16" "UInt32" "UInt64"
                    "Float16" "Float32" "Float64" "BFloat16"
                    "Bool" "String" "StringLiteral"
                    "List" "Dict" "Set" "Tuple" "Optional"
                    "Pointer" "UnsafePointer" "PythonObject"
                    "SIMD" "DType" "object")
                  'symbols)
     . font-lock-type-face)
    (,(regexp-opt '("True" "False" "None" "self") 'symbols)
     . font-lock-constant-face)
    (,(regexp-opt '("print" "input" "len" "range" "str" "int" "float" "bool"
                    "repr" "ord" "chr" "hex" "oct" "bin"
                    "abs" "min" "max" "sum" "pow" "round"
                    "sorted" "reversed" "enumerate" "zip" "map" "filter"
                    "any" "all" "isinstance" "type" "id" "hash"
                    "dir" "vars" "getattr" "setattr" "hasattr" "delattr"
                    "open" "slice" "object"
                    "__mlir_type" "__mlir_op" "__mlir_attr")
                  'symbols)
     . font-lock-builtin-face)
    ("\\<\\(def\\|fn\\)\\>[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (2 font-lock-function-name-face))
    ("^[ \t]*\\(@[a-zA-Z_][a-zA-Z0-9_]*\\)"
     (1 font-lock-preprocessor-face)))
  "Font-lock keywords for `mojo-mode'.")

;;;###autoload
(define-derived-mode mojo-mode prog-mode "Mojo"
  "Major mode for editing Mojo source code."
  :syntax-table mojo-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(mojo-font-lock-keywords))
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mojo\\'" . mojo-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.🔥\\'" . mojo-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(mojo-mode . ("mojo-lsp-server"))))

(provide 'mojo-mode)
;;; mojo-mode.el ends here
