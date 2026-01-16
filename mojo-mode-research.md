# Mojo Major Mode for Emacs: Research & Implementation Guide

## Executive Summary

This document outlines the research findings and implementation plan for creating an MVP Emacs major mode for the Mojo programming language. The MVP scope includes syntax highlighting and LSP integration via Eglot.

---

## 1. Existing Implementations (Inspiration)

### mojo-hl (andcarnivorous/mojo-hl)
- **URL**: https://github.com/andcarnivorous/mojo-hl
- **License**: GPL-3.0
- **Status**: Basic syntax highlighting, ~13 stars
- **Features**: Font-lock keywords, Eglot LSP integration example
- **Limitations**: Minimal, "spotty" LSP experience per author

**Eglot configuration from mojo-hl:**
```elisp
(use-package eglot
  :ensure t
  :defer t
  :hook ((mojo-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(mojo-mode . ("mojo-lsp-server"))))
```

### Official TextMate Grammar
- **URL**: https://github.com/modular/mojo-syntax
- **License**: MIT
- **Notes**: Derived from VS Code's Python syntax (MagicPython). This is the authoritative source for Mojo syntax patterns.

### Tree-sitter Grammars (Future Enhancement)
- https://github.com/lsh/tree-sitter-mojo
- https://github.com/HerringtonDarkholme/tree-sitter-mojo
- https://github.com/garam-kim1/tree-sitter-mojo
- **Notes**: Multiple community efforts exist; could enable `mojo-ts-mode` in future.

---

## 2. Mojo Language Syntax Reference

### Keywords

**Function/Method Declaration:**
- `def` - Python-style dynamic function
- `fn` - Strict typed function (immutable args by default)

**Variable Declaration:**
- `var` - Mutable variable
- `let` - Immutable variable (NOTE: deprecated in recent Mojo versions, check current status)

**Type Definitions:**
- `struct` - Static type (like Rust/C++ struct)
- `trait` - Interface/protocol definition
- `alias` - Compile-time constant/type alias

**Control Flow (Python-compatible):**
- `if`, `elif`, `else`
- `for`, `while`
- `break`, `continue`
- `return`
- `raise`, `try`, `except`, `finally`
- `with`, `as`
- `match`, `case` (pattern matching)

**Imports:**
- `import`, `from`

**Logical/Boolean:**
- `and`, `or`, `not`
- `in`, `is`
- `True`, `False`, `None`

**Other:**
- `pass`
- `lambda`
- `yield`
- `async`, `await`
- `raises` (fn error declaration)
- `owned`, `borrowed`, `inout`, `read` (ownership modifiers)
- `self`

### Mojo-Specific Keywords/Identifiers
- `@parameter` - Compile-time decorator
- `@register_passable`
- `@value`
- `@always_inline`
- `@fieldwise_init`
- `SIMD`, `DType`
- `__mlir_type`, `__mlir_op` (MLIR integration)

### Built-in Types
- `Int`, `UInt`, `Int8`, `Int16`, `Int32`, `Int64`
- `UInt8`, `UInt16`, `UInt32`, `UInt64`
- `Float16`, `Float32`, `Float64`, `BFloat16`
- `Bool`
- `String`, `StringLiteral`
- `List`, `Dict`, `Set`, `Tuple`
- `Optional`
- `Pointer`, `UnsafePointer`
- `PythonObject`

### Comments
- Single-line: `# comment`
- Docstrings: `"""docstring"""`

### Operators
Same as Python plus:
- `^` for ownership transfer (move semantics)

### File Extensions
- `.mojo`
- `.🔥` (fire emoji - officially supported!)

---

## 3. Emacs Major Mode Architecture

### Core Components

#### 3.1 Mode Definition (define-derived-mode)
```elisp
;;;###autoload
(define-derived-mode mojo-mode prog-mode "Mojo"
  "Major mode for editing Mojo source code."
  :syntax-table mojo-mode-syntax-table
  :group 'mojo
  
  ;; Font-lock (syntax highlighting)
  (setq-local font-lock-defaults '(mojo-font-lock-keywords))
  
  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-end "")
  
  ;; Indentation (can delegate to python-mode initially)
  (setq-local indent-line-function #'mojo-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4))
```

#### 3.2 Syntax Table
```elisp
(defvar mojo-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments: # to end of line
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    
    ;; Operators/punctuation
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?~ "." table)
    
    ;; Paired delimiters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    
    ;; Underscore is word constituent
    (modify-syntax-entry ?_ "w" table)
    
    table)
  "Syntax table for `mojo-mode'.")
```

#### 3.3 Font-Lock Keywords
```elisp
(defvar mojo-font-lock-keywords
  `(
    ;; Keywords
    (,(regexp-opt '("def" "fn" "struct" "trait" "alias"
                    "var" "let"
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
    
    ;; Constants
    (,(regexp-opt '("True" "False" "None" "self") 'symbols)
     . font-lock-constant-face)
    
    ;; Built-in types
    (,(regexp-opt '("Int" "UInt" "Int8" "Int16" "Int32" "Int64"
                    "UInt8" "UInt16" "UInt32" "UInt64"
                    "Float16" "Float32" "Float64" "BFloat16"
                    "Bool" "String" "StringLiteral"
                    "List" "Dict" "Set" "Tuple" "Optional"
                    "Pointer" "UnsafePointer" "PythonObject"
                    "SIMD" "DType" "object")
                  'symbols)
     . font-lock-type-face)
    
    ;; Decorators
    ("^[ \t]*\\(@[a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-preprocessor-face)
    
    ;; Function/method definitions
    ("\\<\\(def\\|fn\\)\\>[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (2 font-lock-function-name-face))
    
    ;; Struct/trait definitions
    ("\\<\\(struct\\|trait\\)\\>[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (2 font-lock-type-face))
    
    ;; MLIR intrinsics
    ("\\<__mlir_\\(type\\|op\\|attr\\)\\>" . font-lock-builtin-face))
  "Font-lock keywords for `mojo-mode'.")
```

#### 3.4 Auto-mode Registration
```elisp
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mojo\\'" . mojo-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.🔥\\'" . mojo-mode))
```

---

## 4. LSP Integration

### Mojo LSP Server
- **Binary**: `mojo-lsp-server`
- **Installation**: Comes with Mojo SDK (in `~/.modular/pkg/packages.modular.com_mojo/bin/`)
- **Protocol**: Standard LSP

### Eglot Configuration
```elisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(mojo-mode . ("mojo-lsp-server"))))

;; Optional: Auto-start eglot
(add-hook 'mojo-mode-hook #'eglot-ensure)
```

### Alternative: lsp-mode Configuration
```elisp
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(mojo-mode . "mojo"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("mojo-lsp-server"))
    :activation-fn (lsp-activate-on "mojo")
    :server-id 'mojo-lsp)))
```

### LSP Capabilities Expected
- Diagnostics (errors, warnings)
- Completion
- Hover documentation
- Go to definition
- Find references
- Formatting

---

## 5. Testing Strategy

### Recommended Framework: Buttercup
- BDD-style syntax (`describe`, `it`, `expect`)
- Better CI integration than ERT
- Supports spies/mocks

### Test Categories

#### 5.1 Syntax Table Tests
```elisp
(describe "mojo-mode syntax table"
  (it "identifies comments correctly"
    (with-temp-buffer
      (mojo-mode)
      (insert "# this is a comment")
      (goto-char (point-min))
      (forward-char 2)
      (expect (nth 4 (syntax-ppss)) :to-be-truthy)))
  
  (it "identifies strings correctly"
    (with-temp-buffer
      (mojo-mode)
      (insert "\"hello world\"")
      (goto-char 5)
      (expect (nth 3 (syntax-ppss)) :to-be-truthy))))
```

#### 5.2 Font-Lock Tests
```elisp
(describe "mojo-mode font-lock"
  (it "highlights fn keyword"
    (with-temp-buffer
      (mojo-mode)
      (insert "fn main():")
      (font-lock-ensure)
      (goto-char (point-min))
      (expect (get-text-property (point) 'face)
              :to-equal 'font-lock-keyword-face)))
  
  (it "highlights function name after fn"
    (with-temp-buffer
      (mojo-mode)
      (insert "fn main():")
      (font-lock-ensure)
      (goto-char 4) ; on 'm' of main
      (expect (get-text-property (point) 'face)
              :to-equal 'font-lock-function-name-face))))
```

#### 5.3 Integration Tests
```elisp
(describe "mojo-mode activation"
  (it "activates for .mojo files"
    (with-temp-buffer
      (setq buffer-file-name "test.mojo")
      (set-auto-mode)
      (expect major-mode :to-be 'mojo-mode)))
  
  (it "sets correct comment syntax"
    (with-temp-buffer
      (mojo-mode)
      (expect comment-start :to-equal "# "))))
```

### Running Tests
```bash
# With Cask
cask exec buttercup -L .

# Without Cask
emacs -batch -L . -l buttercup -f buttercup-run-discover
```

### CI Configuration (GitHub Actions)
```yaml
name: CI
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        emacs-version: ['28.2', '29.1', 'snapshot']
    steps:
      - uses: actions/checkout@v4
      - uses: purcell/setup-emacs@master
        with:
          version: ${{ matrix.emacs-version }}
      - uses: conao3/setup-cask@master
      - run: cask install
      - run: cask exec buttercup -L .
```

---

## 6. Project Structure

```
mojo-mode/
├── mojo-mode.el          # Main mode definition
├── test/
│   └── mojo-mode-test.el # Buttercup tests
├── Cask                  # Dependencies
├── Makefile              # Build/test automation
├── README.md
├── LICENSE               # Recommend GPL-3.0
└── .github/
    └── workflows/
        └── ci.yml
```

### Cask File
```
(source gnu)
(source melpa)

(package-file "mojo-mode.el")

(development
 (depends-on "buttercup"))
```

### Makefile
```makefile
.PHONY: test clean

test:
	cask exec buttercup -L .

lint:
	cask exec emacs -batch -L . \
	  --eval "(require 'package-lint)" \
	  -f package-lint-batch-and-exit mojo-mode.el

clean:
	rm -rf .cask
```

---

## 7. Implementation Phases

### Phase 1: MVP (Current Scope)
- [x] Research complete
- [ ] Basic `mojo-mode` with `define-derived-mode`
- [ ] Syntax table for comments and strings
- [ ] Font-lock keywords (keywords, types, functions, decorators)
- [ ] Auto-mode-alist for `.mojo` and `.🔥`
- [ ] Eglot integration
- [ ] Basic test suite
- [ ] Package metadata (header comments)

### Phase 2: Enhanced Editing
- [ ] Indentation function (can start with `python-indent-line`)
- [ ] Imenu support (function/struct navigation)
- [ ] Electric pairs customization
- [ ] Fill paragraph for docstrings

### Phase 3: Advanced Features
- [ ] `mojo-ts-mode` (tree-sitter based)
- [ ] REPL integration
- [ ] Flycheck/Flymake backend (beyond LSP)
- [ ] Project.el integration
- [ ] Code folding (hideshow)

### Phase 4: Ecosystem
- [ ] MELPA submission
- [ ] Integration with `dape` (Debug Adapter Protocol)
- [ ] Org-babel support

---

## 8. Key References

### Emacs Documentation
- [Major Mode Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Major-Mode-Conventions.html)
- [Derived Modes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html)
- [Font Lock Mode](https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Mode.html)
- [Eglot Manual](https://www.gnu.org/software/emacs/manual/html_node/eglot/index.html)
- [ERT Manual](https://www.gnu.org/software/emacs/manual/html_mono/ert.html)

### Tutorials
- [Writing a Simple Major Mode](https://www.omarpolo.com/post/writing-a-major-mode.html)
- [Adding a New Language to Emacs](https://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/)
- [Let's Write a Tree-Sitter Major Mode](https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode)
- [Testing Emacs with Buttercup](http://www.modernemacs.com/post/testing-emacs/)
- [EmacsWiki: Mode Tutorial](https://www.emacswiki.org/emacs/ModeTutorial)

### Mojo Documentation
- [Mojo Manual](https://docs.modular.com/mojo/manual/)
- [Mojo Language Basics](https://docs.modular.com/mojo/manual/basics/)
- [Mojo TextMate Grammar](https://github.com/modular/mojo-syntax)

### Testing
- [Buttercup Documentation](https://github.com/jorgenschaefer/emacs-buttercup)

---

## 9. Notes for Claude Code

When implementing this mode:

1. **Start minimal**: Get basic highlighting working before adding features
2. **Derive from prog-mode**: Provides sensible defaults for programming modes
3. **Use regexp-opt**: Efficiently generates keyword regex
4. **Test incrementally**: Write tests alongside implementation
5. **Package conventions**: Include proper header comments for MELPA compatibility
6. **Indentation**: Can initially reuse `python-indent-line-function` since Mojo uses Python-style indentation
7. **LSP**: Eglot is built-in from Emacs 29; prefer it over lsp-mode for simplicity

### Sample Files for Testing

Create test files with various Mojo constructs:
- Functions (`def`, `fn`)
- Structs and traits
- Decorators
- Type annotations
- Ownership modifiers
- MLIR intrinsics
- Docstrings
- Comments

---

*Document generated: January 2026*
*Target: Emacs 28.1+*
