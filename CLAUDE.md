# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `mojo-mode`, an Emacs major mode for editing Mojo programming language files. It provides syntax highlighting and LSP integration via Eglot.

## Build and Test Commands

```bash
# Install dependencies (requires Cask)
make install

# Run all tests (24 specs: unit + visual regression)
make test

# Run a single test file
cask exec buttercup -L . test/mojo-mode-test.el

# Regenerate visual test fixtures from Mojo docs (requires playwright)
pip install playwright && playwright install chromium
make scrape-fixtures
```

## Architecture

### Core Files

- `mojo-mode.el` - The entire mode implementation (~90 lines). Contains:
  - `mojo-mode-syntax-table` - Comment and string syntax
  - `mojo-font-lock-keywords` - Highlighting rules for keywords, types, builtins, constants, functions, decorators
  - `mojo-mode` - The derived major mode (from `prog-mode`)
  - Eglot integration via `eglot-server-programs`

### Test Structure

- `test/mojo-mode-test.el` - Unit tests using Buttercup (BDD-style)
- `test/mojo-mode-visual-test.el` - Visual regression tests comparing against Mojo docs tokenization
- `test/fixtures/*.json` - Token data scraped from docs.modular.com

### Visual Regression Testing

The visual tests compare our font-lock highlighting against Prism.js tokens from the official Mojo documentation:

1. Fixtures contain `{code, tokens}` where tokens have `{start, end, type, text}`
2. `mojo-test-token-face-map` maps Prism token types to Emacs faces
3. `mojo-test-acceptable-differences` allows intentional deviations (e.g., we highlight `String` as a type, docs don't)

## Development Notes

- Follow strict TDD: write failing test first, then implement
- Target Emacs 30.1+ (no backward compatibility needed)
- Use `regexp-opt` with `'symbols` for keyword matching
- LSP server is `mojo-lsp-server` (comes with Mojo SDK)
