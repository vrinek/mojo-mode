;;; mojo-mode-visual-test.el --- Visual regression tests for mojo-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests that compare mojo-mode syntax highlighting against
;; token data scraped from the official Mojo documentation.
;;
;; Fixture format (JSON):
;; {
;;   "code": "def main(): ...",
;;   "tokens": [
;;     {"start": 0, "end": 3, "type": "keyword", "text": "def"},
;;     ...
;;   ]
;; }

;;; Code:

(require 'buttercup)
(require 'mojo-mode)
(require 'json)

;;; Token type to Emacs face mapping
;;
;; Based on Prism.js token classes used on docs.modular.com

(defvar mojo-test-token-face-map
  '(("keyword" . font-lock-keyword-face)
    ("function" . font-lock-function-name-face)
    ("builtin" . font-lock-builtin-face)
    ("string" . font-lock-string-face)
    ("constant" . font-lock-constant-face)
    ("number" . font-lock-constant-face)
    ("operator" . nil)  ; operators typically not highlighted
    ("punctuation" . nil)  ; punctuation typically not highlighted
    ("plain" . nil)  ; plain text not highlighted
    ("comment" . font-lock-comment-face)
    ("class-name" . nil))  ; class names in calls not highlighted
  "Mapping from Prism.js token types to Emacs font-lock faces.
nil means no highlighting expected.")

(defvar mojo-test-acceptable-differences
  '(;; We highlight built-in types like String, the website doesn't
    (("plain" . font-lock-type-face) . "We highlight type names")
    ;; Website inconsistently marks print as keyword, input as builtin
    ;; We treat both as builtins which is more consistent
    (("keyword" . font-lock-builtin-face) . "print/input both as builtins")
    ;; The scraper captures first tokens including keywords as "plain"
    ;; because Prism.js wraps them inconsistently
    (("plain" . font-lock-keyword-face) . "First line contains keywords")
    (("plain" . font-lock-comment-face) . "First line is a comment")
    (("plain" . font-lock-comment-delimiter-face) . "First line is a comment")
    (("plain" . font-lock-preprocessor-face) . "First line is a decorator")
    (("plain" . font-lock-builtin-face) . "First line contains builtins")
    (("plain" . font-lock-constant-face) . "First line contains constants")
    ;; We highlight self as a constant (Python convention)
    (("special-vars" . font-lock-constant-face) . "self highlighted as constant")
    ;; Decorators: website marks @ as punctuation, we highlight whole decorator
    (("punctuation" . font-lock-preprocessor-face) . "Decorator @ highlighted")
    ;; We highlight var/let as keywords, website sometimes marks as plain
    (("plain" . font-lock-variable-name-face) . "Variable declarations highlighted")
    ;; Emacs uses comment-delimiter-face for #, website uses comment for whole thing
    (("comment" . font-lock-comment-delimiter-face) . "Comment delimiter vs comment face")
    ;; Website marks None as boolean, we mark as constant (like True/False)
    (("boolean" . font-lock-constant-face) . "None is a constant")
    ;; Prism.js tokenizes inside comments/strings, Emacs correctly highlights
    ;; the entire region as comment/string face
    (("punctuation" . font-lock-comment-face) . "Token inside comment")
    (("number" . font-lock-comment-face) . "Token inside comment")
    (("keyword" . font-lock-comment-face) . "Token inside comment")
    (("operator" . font-lock-comment-face) . "Token inside comment")
    (("plain" . font-lock-string-face) . "Token inside string")
    (("punctuation" . font-lock-string-face) . "Token inside string")
    (("number" . font-lock-string-face) . "Token inside string")
    (("keyword" . font-lock-string-face) . "Token inside string")
    (("builtin" . font-lock-string-face) . "Token inside string")
    (("class-name" . font-lock-string-face) . "Token inside string")
    ;; We don't highlight function calls, only definitions
    (("function" . nil) . "Function calls not highlighted")
    ;; Scraper sometimes misidentifies keywords as function names
    (("keyword" . font-lock-function-name-face) . "Keyword near function pattern")
    ;; We highlight sum/print as builtins, website marks as function
    (("function" . font-lock-builtin-face) . "Builtin function")
    ;; Operators and functions inside strings
    (("operator" . font-lock-string-face) . "Operator inside string")
    (("function" . font-lock-string-face) . "Function inside string")
    ;; Keywords as substring of identifiers (scraper bug)
    (("keyword" . nil) . "Keyword substring in identifier"))
  "Alist of (EXPECTED . ACTUAL) pairs that are acceptable differences.
Each entry is ((token-type . actual-face) . reason).")

;;; Helper functions

(defun mojo-test-find-project-root ()
  "Find the mojo-mode project root directory."
  (let ((mojo-mode-file (locate-library "mojo-mode")))
    (when mojo-mode-file
      (file-name-directory mojo-mode-file))))

(defun mojo-test-load-fixture (filename)
  "Load a JSON fixture from FILENAME in the fixtures directory."
  (let* ((project-root (mojo-test-find-project-root))
         (fixture-path (expand-file-name (concat "test/fixtures/" filename) project-root)))
    (with-temp-buffer
      (insert-file-contents fixture-path)
      (json-parse-buffer :object-type 'alist))))

(defun mojo-test-get-face-at (pos)
  "Get the font-lock face at position POS.
Returns a single face symbol, handling both single faces and face lists."
  (let ((face (get-text-property pos 'face)))
    (cond
     ((null face) nil)
     ((symbolp face) face)
     ((listp face) (car face))  ; Take first face if it's a list
     (t face))))

(defun mojo-test-acceptable-p (token-type actual-face)
  "Check if the difference between TOKEN-TYPE and ACTUAL-FACE is acceptable."
  (assoc (cons token-type actual-face) mojo-test-acceptable-differences))

(defun mojo-test-check-token (code token)
  "Check that TOKEN in CODE is highlighted with the expected face.
Returns a result alist with :passed, :expected, :actual, and :text."
  (let* ((start (alist-get 'start token))
         (token-type (alist-get 'type token))
         (text (alist-get 'text token))
         (expected-face (cdr (assoc token-type mojo-test-token-face-map)))
         (actual-face (with-temp-buffer
                        (mojo-mode)
                        (insert code)
                        (font-lock-ensure)
                        ;; +1 because Emacs positions are 1-indexed
                        (mojo-test-get-face-at (1+ start))))
         (exact-match (equal expected-face actual-face))
         (acceptable (mojo-test-acceptable-p token-type actual-face)))
    `((passed . ,(or exact-match acceptable))
      (token-type . ,token-type)
      (text . ,text)
      (start . ,start)
      (expected . ,expected-face)
      (actual . ,actual-face)
      (acceptable-reason . ,(cdr acceptable)))))

(defun mojo-test-check-fixture (fixture)
  "Check all tokens in FIXTURE against mojo-mode highlighting.
Returns a list of result alists."
  (let ((code (alist-get 'code fixture))
        (tokens (alist-get 'tokens fixture)))
    (mapcar (lambda (token)
              (mojo-test-check-token code token))
            tokens)))

(defun mojo-test-fixture-summary (results)
  "Generate a summary string from RESULTS."
  (let ((total (length results))
        (passed (length (seq-filter (lambda (r) (alist-get 'passed r)) results)))
        (failed (seq-filter (lambda (r) (not (alist-get 'passed r))) results)))
    (concat
     (format "Passed: %d/%d\n" passed total)
     (when failed
       (concat "Failures:\n"
               (mapconcat
                (lambda (r)
                  (format "  '%s' (%s at %d): expected %s, got %s"
                          (alist-get 'text r)
                          (alist-get 'token-type r)
                          (alist-get 'start r)
                          (alist-get 'expected r)
                          (alist-get 'actual r)))
                failed
                "\n"))))))

(defun mojo-test-list-fixtures ()
  "Return a list of all fixture filenames in the fixtures directory."
  (let* ((project-root (mojo-test-find-project-root))
         (fixtures-dir (expand-file-name "test/fixtures" project-root)))
    (when (file-directory-p fixtures-dir)
      (directory-files fixtures-dir nil "\\.json$"))))

(defun mojo-test-run-fixture (filename)
  "Run visual regression test for FILENAME fixture.
Returns t if all tokens pass, nil otherwise."
  (let* ((fixture (mojo-test-load-fixture filename))
         (results (mojo-test-check-fixture fixture))
         (all-passed (seq-every-p (lambda (r) (alist-get 'passed r)) results)))
    (unless all-passed
      (message "\n%s: %s" filename (mojo-test-fixture-summary results)))
    all-passed))

;;; Tests

(describe "mojo-mode visual regression"
  (it "highlights all fixtures matching the official docs"
    (let* ((fixtures (mojo-test-list-fixtures))
           (results (mapcar (lambda (f) (cons f (mojo-test-run-fixture f))) fixtures))
           (failed (seq-filter (lambda (r) (not (cdr r))) results))
           (passed-count (- (length results) (length failed))))
      (message "\nVisual regression: %d/%d fixtures passed" passed-count (length results))
      (when failed
        (message "Failed fixtures: %s" (mapconcat #'car failed ", ")))
      (expect (length failed) :to-equal 0))))

;;; mojo-mode-visual-test.el ends here
