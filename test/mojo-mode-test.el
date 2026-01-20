;;; mojo-mode-test.el --- Tests for mojo-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for mojo-mode.

;;; Code:

(require 'buttercup)
(require 'mojo-mode)

(describe "mojo-mode"
  (it "is a major mode"
    (expect (fboundp 'mojo-mode) :to-be-truthy))

  (it "activates for .mojo files"
    (with-temp-buffer
      (setq buffer-file-name "test.mojo")
      (set-auto-mode)
      (expect major-mode :to-be 'mojo-mode)))

  (it "activates for .🔥 files"
    (with-temp-buffer
      (setq buffer-file-name "test.🔥")
      (set-auto-mode)
      (expect major-mode :to-be 'mojo-mode))))

(describe "mojo-mode syntax table"
  (it "recognizes # as comment start"
    (with-temp-buffer
      (mojo-mode)
      (insert "# this is a comment")
      (goto-char 3)
      (expect (nth 4 (syntax-ppss)) :to-be-truthy)))

  (it "sets comment-start to \"# \""
    (with-temp-buffer
      (mojo-mode)
      (expect comment-start :to-equal "# ")))

  (it "recognizes double-quoted strings"
    (with-temp-buffer
      (mojo-mode)
      (insert "\"hello world\"")
      (goto-char 5)
      (expect (nth 3 (syntax-ppss)) :to-be-truthy)))

  (it "recognizes single-quoted strings"
    (with-temp-buffer
      (mojo-mode)
      (insert "'hello world'")
      (goto-char 5)
      (expect (nth 3 (syntax-ppss)) :to-be-truthy))))

(describe "mojo-mode font-lock"
  (it "highlights fn as keyword"
    (with-temp-buffer
      (mojo-mode)
      (insert "fn main():")
      (font-lock-ensure)
      (expect (get-text-property 1 'face) :to-equal 'font-lock-keyword-face)))

  (it "highlights def as keyword"
    (with-temp-buffer
      (mojo-mode)
      (insert "def foo():")
      (font-lock-ensure)
      (expect (get-text-property 1 'face) :to-equal 'font-lock-keyword-face)))

  (it "highlights struct as keyword"
    (with-temp-buffer
      (mojo-mode)
      (insert "struct Point:")
      (font-lock-ensure)
      (expect (get-text-property 1 'face) :to-equal 'font-lock-keyword-face)))

  (it "highlights Int as type"
    (with-temp-buffer
      (mojo-mode)
      (insert "var x: Int = 5")
      (font-lock-ensure)
      (expect (get-text-property 8 'face) :to-equal 'font-lock-type-face)))

  (it "highlights Bool as type"
    (with-temp-buffer
      (mojo-mode)
      (insert "var flag: Bool")
      (font-lock-ensure)
      (expect (get-text-property 11 'face) :to-equal 'font-lock-type-face)))

  (it "highlights function name after fn"
    (with-temp-buffer
      (mojo-mode)
      (insert "fn main():")
      (font-lock-ensure)
      ;; "fn main" - position 4 is 'm' of main
      (expect (get-text-property 4 'face) :to-equal 'font-lock-function-name-face)))

  (it "highlights function name after def"
    (with-temp-buffer
      (mojo-mode)
      (insert "def foo():")
      (font-lock-ensure)
      ;; "def foo" - position 5 is 'f' of foo
      (expect (get-text-property 5 'face) :to-equal 'font-lock-function-name-face)))

  (it "highlights decorators"
    (with-temp-buffer
      (mojo-mode)
      (insert "@value\nstruct Point:")
      (font-lock-ensure)
      ;; position 1 is '@' of @value
      (expect (get-text-property 1 'face) :to-equal 'font-lock-preprocessor-face)))

  (it "highlights True as constant"
    (with-temp-buffer
      (mojo-mode)
      (insert "var x = True")
      (font-lock-ensure)
      (expect (get-text-property 9 'face) :to-equal 'font-lock-constant-face)))

  (it "highlights None as constant"
    (with-temp-buffer
      (mojo-mode)
      (insert "var x = None")
      (font-lock-ensure)
      (expect (get-text-property 9 'face) :to-equal 'font-lock-constant-face)))

  (it "highlights print as builtin"
    (with-temp-buffer
      (mojo-mode)
      (insert "print(x)")
      (font-lock-ensure)
      (expect (get-text-property 1 'face) :to-equal 'font-lock-builtin-face)))

  (it "highlights input as builtin"
    (with-temp-buffer
      (mojo-mode)
      (insert "input(\"prompt\")")
      (font-lock-ensure)
      (expect (get-text-property 1 'face) :to-equal 'font-lock-builtin-face))))

(describe "mojo-mode indentation"
  (it "uses 4-space indentation"
    (with-temp-buffer
      (mojo-mode)
      (expect tab-width :to-equal 4)))

  (it "does not use tabs"
    (with-temp-buffer
      (mojo-mode)
      (expect indent-tabs-mode :to-be nil)))

  (it "indents after colon"
    (with-temp-buffer
      (mojo-mode)
      (insert "def main():\n")
      (mojo-indent-line)
      (expect (current-indentation) :to-equal 4)))

  (it "indents after fn declaration"
    (with-temp-buffer
      (mojo-mode)
      (insert "fn foo():\n")
      (mojo-indent-line)
      (expect (current-indentation) :to-equal 4)))

  (it "indents after if statement"
    (with-temp-buffer
      (mojo-mode)
      (insert "if x > 0:\n")
      (mojo-indent-line)
      (expect (current-indentation) :to-equal 4)))

  (it "maintains indentation for regular lines"
    (with-temp-buffer
      (mojo-mode)
      (insert "def main():\n    x = 1\n")
      (mojo-indent-line)
      (expect (current-indentation) :to-equal 4)))

  (it "dedents for return statement"
    (with-temp-buffer
      (mojo-mode)
      (insert "def main():\n    x = 1\n    return")
      (beginning-of-line)
      (mojo-indent-line)
      (expect (current-indentation) :to-equal 0)))

  (it "indents nested blocks"
    (with-temp-buffer
      (mojo-mode)
      (insert "def main():\n    if x:\n")
      (mojo-indent-line)
      (expect (current-indentation) :to-equal 8))))

(describe "mojo-mode eglot integration"
  (it "registers mojo-mode in eglot-server-programs"
    (require 'eglot)
    (expect (assoc 'mojo-mode eglot-server-programs) :to-be-truthy))

  (it "uses mojo-lsp-server as the LSP server"
    (require 'eglot)
    (let ((entry (assoc 'mojo-mode eglot-server-programs)))
      (expect (cdr entry) :to-equal '("mojo-lsp-server")))))

;;; mojo-mode-test.el ends here
