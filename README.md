# koopa-mode
An Emacs major mode for Microsoft PowerShell

## Features

* Syntax highlighting and indentation
* The ability to spawn – and send code to – a PowerShell process in a new buffer
* Autocompletion for built-in cmdlets

## Prerequisites

### `company-mode`

`koopa-mode` uses [`company-mode`](https://company-mode.github.io/manual/Getting-Started.html) for autocompletion. To install `company-mode`, use the following command in Emacs:

```
M-x package-install RET company RET
```


## Installation

### Manual

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
| `koopa-newline-and-indent`        | `C-<return>` | Indents to the appropriate indentation level and inserts a newline  |
| `koopa-run-powershell`            | `C-c C-p`    | Spawns a PowerShell process in a new buffer                         |
| `koopa-send-line-to-powershell`   | `C-c C-c`    | Send the current line to the PowerShell process                     |
| `koopa-send-buffer-to-powershell` | `C-c C-b`    | Send the entire buffer to the PowerShell process                    |
| `koopa-auto-indent`               | `C-<tab>`    | Automatically indents the line to the appropriate indentation level |

## Demo

![Demo of koopa-mode](screenshots/koopa-mode-demo.GIF "Demo of koopa-mode")

## Known Issues

- On Windows, autocompletion sometimes adds whitespace at the end the cmdlet
  - In the meantime, a workaround is to autocomplete using `TAB` and then pressing `C-g`
  - The additional whitespace can also affect indentation, so if a line is not indenting properly, then you may have extra whitespace at the end

## License

[MIT](https://choosealicense.com/licenses/mit/)
