# koopa-mode
An Emacs major mode for Microsoft PowerShell

[![melpazoid](https://github.com/sch0lars/koopa-mode/actions/workflows/melpazoid.yml/badge.svg)](https://github.com/sch0lars/koopa-mode/actions/workflows/melpazoid.yml)
[![GNU Emacs](https://img.shields.io/static/v1?logo=gnuemacs&logoColor=fafafa&label=Made%20for&message=GNU%20Emacs&color=7F5AB6&style=flat)](https://www.gnu.org/software/emacs/)
[![MELPA](https://melpa.org/packages/koopa-mode-badge.svg)](https://melpa.org/#/koopa-mode)

## Features

* Syntax highlighting and indentation
* The ability to spawn – and send code to – a PowerShell process in a separate buffer
* Autocompletion for both built-in and user-defined cmdlets and variables, .NET types, and .NET type methods

## Prerequisites

### `company-mode`

`koopa-mode` uses [`company-mode`](https://company-mode.github.io/manual/Getting-Started.html) for autocompletion. To install `company-mode`, use the following command in Emacs:

```
M-x package-install RET company RET
```

## Installation

### MELPA

To install `koopa-mode` via [MELPA](https://melpa.org/#/getting-started), use the following command:

`M-x package-install RET koopa-mode RET`

Then, add the following to your `init.el`:

`(require 'koopa-mode)`

### Manual

To install `koopa-mode` manually, first clone the repo.

```bash
git clone https://github.com/sch0lars/koopa-mode.git
```

Then, add the following to your `init.el`:

```emacs-lisp
(add-to-list 'load-path "/path/to/koopa-mode")
(require 'koopa-mode)
```

## File Associations

To automatically associate PowerShell files with `koopa-mode`, add the following to your `init.el`:

```emacs-lisp
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . koopa-mode))
```

## Configuration

### Adding PowerShell Command Line Arguments

By default, the only command line argument passed to PowerShell is `-NoProfile`. To add command line arguments to PowerShell, you must modify the `koopa-powershell-cli-arguments` variable. 

For example, to add the `-NoLogo` argument, you would add the following command to your `init.el`:

```emacs-lisp
(add-to-list 'koopa-powershell-cli-arguments "-NoLogo")
```

### Specifying the PowerShell Executable

In order to spawn a PowerShell process, PowerShell should be in your system PATH. `koopa-mode` will automatically search for the PowerShell executable, which is set in the variable `koopa-powershell-executable`. 


`koopa-mode` expects one of two values:

* `powershell` on Windows; or
* `pwsh` on *nix

To change the value of the executable's name, use the following command:

```emacs-lisp
(setq koopa-powershell-executable "name_of_executable")
```

If `koopa-mode` cannot find the PowerShell executable, it will generate the error `Searching for program: No such file or directory, <executable name>`.

### Specifying the Operating System

In order to handle escape sequences popping up in the shell on *nix systems, `koopa-mode` attempts to guess what operating system you are using based on the executable it finds. If you are on a *nix systems and experience random escape sequence characters popping up when you spawn a shell using `koopa-run-powershell`, ensure your `koopa-is-running-on-windows` variable is set accordingly. **You generally should not have to modify this variable**.

To tell `koopa-mode` you are using Windows, use the following command:

```emacs-lisp
(setq koopa-is-running-on-windows t)
```

To tell `koopa-mode` you are using *nix, use the following command:

```emacs-lisp
(setq koopa-is-running-on-windows nil)
```


## Keybindings

| Command                           | Keybinding   | Description                                                         |
|-----------------------------------|--------------|---------------------------------------------------------------------|
| `koopa-indent-line`               | `<tab>`      | Indents the line by `koopa-indent-offset`                           |
| `koopa-dedent-line`               | `<backtab>`  | Dedents the line by `koopa-indent-offset`                           |
| `koopa-auto-indent`               | `C-c <tab>`  | Automatically indents the line to the appropriate indentation level |
| `koopa-newline-and-indent`        | `C-<return>` | Indents to the appropriate indentation level and inserts a newline  |
| `koopa-company-backend`           | `C-x <tab>`  | Triggers the company backend                                        |
| `koopa-run-powershell`            | `C-c C-p`    | Spawns a PowerShell process in a new buffer                         |
| `koopa-send-line-to-powershell`   | `C-c C-c`    | Send the current line to the PowerShell process                     |
| `koopa-send-buffer-to-powershell` | `C-c C-b`    | Send the entire buffer to the PowerShell process                    |

## Demo

![Demo of koopa-mode](screenshots/koopa-mode-demo.GIF "Demo of koopa-mode")

## Known Issues

- `koopa-mode` does not use a traditional backend for `company` completion. Instead, when you assign a variable, it is run through PowerShell and its methods are extracted. Therefore, **you may notice that company completion sometimes has error messages in its suggestions**.
- Similar to the previous issue, you may also notice that a large script may briefly lag for a moment when you insert a newline. That is because of the underlying PowerShell processes that are run in the background to supply code completion features. If you just want the syntax highlighting and not the `company` completion, you can disable `company-mode`. You should not experience this issue with smaller scripts.
- ~~On Windows, autocompletion sometimes adds whitespace at the end of the cmdlet.~~

## License

[GNU General Public License](https://www.gnu.org/licenses/gpl-3.0.en.html)
