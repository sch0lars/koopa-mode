# koopa-mode
An Emacs major mode for Microsoft PowerShell

## Installation

Currently, `koopa-mode` has to be manually installed.

To install, first clone the repo.

```bash
git clone https://github.com/sch0lars/koopa-mode.git
```

Then, add the following to your `init.el`:

```emacs-lisp
(add-to-list 'load-path "/path/to/koopa-mode")
(require 'koopa-mode)
```

**Optional**: To automatically associate PowerShell files with `koopa-mode`, add the following to your `init.el`:

```emacs-lisp
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . koopa-mode))
```

## Demo

![Demo of koopa-mode](screenshots/koopa-mode-demo.GIF "Demo of koopa-mode")

## Known Issues

- On Windows, autocompletion adds whitespace at the end the cmdlet
  - In the meantime, a workaround is to autocomplete using `TAB` and then pressing `C-g`
- Nested indentation is buggy

## License

[MIT](https://choosealicense.com/licenses/mit/)
