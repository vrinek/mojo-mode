# mojo-mode

An Emacs major mode for editing [Mojo](https://www.modular.com/mojo) source code.

## Features

- Syntax highlighting for keywords, types, builtins, constants, and decorators
- Function name highlighting after `def`/`fn`
- File associations for `.mojo` and `.🔥`
- LSP integration via Eglot (requires `mojo-lsp-server`)
- 4-space indentation

## Requirements

- Emacs 30.1+
- [Mojo](https://docs.modular.com/mojo/manual/install) (for LSP support)

## Installation

### Manual

```elisp
(add-to-list 'load-path "/path/to/mojo-mode")
(require 'mojo-mode)
```

### use-package

```elisp
(use-package mojo-mode
  :load-path "/path/to/mojo-mode"
  :hook (mojo-mode . eglot-ensure))
```

## LSP Support

The mode automatically registers `mojo-lsp-server` with Eglot. To enable LSP:

```elisp
(add-hook 'mojo-mode-hook #'eglot-ensure)
```

Make sure `mojo-lsp-server` is in your PATH (it comes with the Mojo SDK).

## Development

```bash
# Install dependencies
make install

# Run tests
make test
```

## License

GPL-3.0
