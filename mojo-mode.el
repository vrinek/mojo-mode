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

;; Keywords derived from modular/mojo-syntax TextMate grammar
(defvar mojo-font-lock-keywords
  `(;; Keywords: control flow + declarations
    (,(regexp-opt '(;; Control flow (from TextMate keyword.control.flow)
                    "if" "elif" "else" "for" "while"
                    "break" "continue" "return" "pass"
                    "raise" "try" "except" "finally"
                    "with" "as" "match" "case"
                    "assert" "del" "yield" "await"
                    ;; Declarations (from TextMate storage.type)
                    "def" "fn" "struct" "trait" "class" "alias" "lambda"
                    ;; Storage modifiers (from TextMate storage.modifier)
                    "var" "let" "global" "nonlocal"
                    "owned" "borrowed" "inout" "ref" "mut"
                    "raises" "capturing" "comptime"
                    ;; Import (from TextMate keyword.control.import)
                    "import" "from"
                    ;; Logical operators (from TextMate keyword.operator.logical)
                    "and" "or" "not" "in" "is"
                    ;; Async (from TextMate storage.type.function.async)
                    "async")
                  'symbols)
     . font-lock-keyword-face)
    ;; Types: Mojo-specific types (capitalized)
    (,(regexp-opt '("Int" "UInt" "Int8" "Int16" "Int32" "Int64"
                    "UInt8" "UInt16" "UInt32" "UInt64"
                    "Float16" "Float32" "Float64" "BFloat16"
                    "Bool" "String" "StringLiteral" "StringRef"
                    "List" "Dict" "Set" "Tuple" "Optional"
                    "Pointer" "UnsafePointer" "PythonObject"
                    "SIMD" "DType" "AnyType" "Movable" "Copyable"
                    "Error" "Coroutine")
                  'symbols)
     . font-lock-type-face)
    ;; Types: Python builtin types (from TextMate support.type)
    ;; Note: list excluded - Prism.js marks it as builtin function
    (,(regexp-opt '("bool" "bytes" "bytearray" "classmethod" "complex"
                    "dict" "float" "frozenset" "int" "object"
                    "property" "set" "slice" "staticmethod" "str"
                    "tuple" "type" "super")
                  'symbols)
     . font-lock-type-face)
    ;; Constants (from TextMate constant.language)
    (,(regexp-opt '("True" "False" "None" "NotImplemented" "Ellipsis")
                  'symbols)
     . font-lock-constant-face)
    ;; Special variables (from TextMate variable.language.special)
    (,(regexp-opt '("self" "cls") 'symbols)
     . font-lock-variable-name-face)
    ;; Builtins (from TextMate support.function.builtin)
    (,(regexp-opt '("__import__" "abs" "aiter" "all" "any" "anext" "ascii"
                    "bin" "breakpoint" "callable" "chr" "compile"
                    "copyright" "credits" "delattr" "dir" "divmod"
                    "enumerate" "eval" "exec" "exit" "filter" "format"
                    "getattr" "globals" "hasattr" "hash" "help" "hex"
                    "id" "input" "isinstance" "issubclass" "iter" "len"
                    "license" "list" "locals" "map" "max" "memoryview" "min"
                    "next" "oct" "open" "ord" "pow" "print" "quit"
                    "range" "repr" "reversed" "round" "setattr" "sorted"
                    "sum" "vars" "zip"
                    ;; Mojo-specific (from TextMate support.type for MLIR)
                    "__mlir_attr" "__mlir_op" "__mlir_type")
                  'symbols)
     . font-lock-builtin-face)
    ;; Numbers: integers (decimal, hex, octal, binary) and floats
    ("\\<0[xX][0-9a-fA-F_]+\\>" . font-lock-constant-face)
    ("\\<0[oO][0-7_]+\\>" . font-lock-constant-face)
    ("\\<0[bB][01_]+\\>" . font-lock-constant-face)
    ("\\<[0-9][0-9_]*\\(?:\\.[0-9_]+\\)?\\(?:[eE][+-]?[0-9_]+\\)?\\>"
     . font-lock-constant-face)
    ;; Function definitions
    ("\\<\\(def\\|fn\\)\\>[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (2 font-lock-function-name-face))
    ;; Struct/trait/class definitions
    ("\\<\\(struct\\|trait\\|class\\)\\>[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (2 font-lock-type-face))
    ;; Decorators
    ("^[ \t]*\\(@[a-zA-Z_][a-zA-Z0-9_]*\\)"
     (1 font-lock-preprocessor-face))
    ;; Magic methods and variables (from TextMate support.function.magic)
    ("\\<__[a-zA-Z_][a-zA-Z0-9_]*__\\>" . font-lock-builtin-face))
  "Font-lock keywords for `mojo-mode'.
Derived from modular/mojo-syntax TextMate grammar.")

(defun mojo-indent-line ()
  "Indent current line as Mojo code."
  (interactive)
  (let ((indent (mojo-calculate-indentation)))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))

(defun mojo-calculate-indentation ()
  "Calculate the indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        0
      (let ((prev-indent 0)
            (prev-line-ends-with-colon nil)
            (curr-line-dedent nil))
        ;; Check if current line starts with dedent keyword
        (when (looking-at "^[ \t]*\\(return\\|pass\\|break\\|continue\\|raise\\|else\\|elif\\|except\\|finally\\)\\b")
          (setq curr-line-dedent t))
        ;; Find previous non-blank line
        (forward-line -1)
        (while (and (not (bobp))
                    (looking-at "^[ \t]*$"))
          (forward-line -1))
        (setq prev-indent (current-indentation))
        ;; Check if previous line ends with colon (ignoring comments)
        (end-of-line)
        (skip-chars-backward " \t")
        (when (and (> (point) (line-beginning-position))
                   (save-excursion
                     (backward-char 1)
                     (not (nth 4 (syntax-ppss)))))  ; not in comment
          (setq prev-line-ends-with-colon (eq (char-before) ?:)))
        ;; Calculate indentation
        (cond
         (prev-line-ends-with-colon
          (+ prev-indent tab-width))
         (curr-line-dedent
          (max 0 (- prev-indent tab-width)))
         (t prev-indent))))))

;;;###autoload
(define-derived-mode mojo-mode prog-mode "Mojo"
  "Major mode for editing Mojo source code."
  :syntax-table mojo-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(mojo-font-lock-keywords))
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local indent-line-function #'mojo-indent-line)
  (setq-local electric-indent-chars (cons ?: electric-indent-chars)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mojo\\'" . mojo-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.🔥\\'" . mojo-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(mojo-mode . ("mojo-lsp-server"))))

(provide 'mojo-mode)
;;; mojo-mode.el ends here
