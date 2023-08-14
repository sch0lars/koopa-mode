;;; koopa-mode.el --- A major mode for Microsoft PowerShell

;; Author: Tyler Hooks
;; URL: https://github.com/sch0lars/koopa-mode
;; Version: 1.2.1
;; Compatibility: GNU Emacs 27.x
;; Keywords: powershell, convenience
;; Package-Requires: ((company "0.9.13") (emacs "27.1"))


;; Copyright (C) 2023 Tyler Hooks
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This file provides `koopa-mode', a major mode for Microsoft PowerShell
;;
;; Usage:
;;
;;     To manually install `koopa-mode', add the following to your init.el:
;;
;;     (add-to-list 'load-path "/path/to/koopa-mode")
;;     (require 'koopa-mode)
;;
;;
;;     To associate PowerShell files with `koopa-mode', add the following
;;     to your init.el:
;;
;;     (add-to-list 'auto-mode-alist '("\\.ps1\\'" . koopa-mode))


;;; Code:

(require 'cl-lib)
(require 'company)

;; Define `koopa-mode'
(define-derived-mode koopa-mode prog-mode "koopa-mode"
  "A major mode for editing Microsoft PowerShell scripts."
  ;; Set the syntax table
  (setq-local syntax-table koopa-mode-syntax-table)
  ;; Set the font-lock keywords
  (setq-local font-lock-defaults '(koopa-mode-font-lock-keywords))
  ;; Make font-lock keywords case-insensitive
  (set (make-local-variable 'font-lock-defaults)
       '(koopa-mode-font-lock-keywords nil t))
  ;; Keybindings
  (local-set-key (kbd "RET") 'newline)
  (local-set-key (kbd "TAB") 'koopa-indent-line)
  (local-set-key (kbd "C-<return>") 'koopa-newline-and-indent)
  (local-set-key (kbd "<backtab>") 'koopa-dedent-line)
  (local-set-key (kbd "C-c TAB") 'koopa-auto-indent)
  (local-set-key (kbd "C-x m") 'koopa-company-backend)
  (local-set-key (kbd "C-c C-p") 'koopa-run-powershell)
  (local-set-key (kbd "C-c C-c") 'koopa-send-line-to-powershell)
  (local-set-key (kbd "C-c C-b") 'koopa-send-buffer-to-powershell)

  ;; Set up `company-mode' in `koopa-mode'
  (add-hook 'koopa-mode-hook
            (lambda ()
              (company-mode t)
              (setq-local company-backends '(koopa-company-backend))))

  ;;; Hooks
  ;; Create a hook for newlines
  (setq-local koopa-newline-hook nil
    "A hook to check if the RETURN key was pressed and a newline was inserted.")
  ;; Create a hook for double colons
  (setq-local koopa-static-member-hook nil
    "A hook to check when a .NET type is being invoked.")
  ;; Add hook to check for new user-defined cmdlets and variables
  (add-hook 'post-command-hook #'koopa-monitor-code-changes)
  ;; Add hook to get member methods on save
  (add-hook 'post-command-hook #'koopa-update-newline-hook)
  (add-hook 'koopa-newline-hook #'koopa-powershell-get-member-methods)
  ;; Cleanup deleted methods from `koopa-powershell-member-methods' on a newline
  (add-hook 'koopa-newline-hook #'koopa-cleanup-powershell-member-methods)
  ;; Check if a .NET type is being invoked and add its methods to `koopa-powershell-dotnet-methods'
  (add-hook 'post-command-hook #'koopa-update-static-member-hook)
  (add-hook 'koopa-static-member-hook #'koopa-update-powershell-dotnet-methods))

;; Define the syntax table
(defconst koopa-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comment syntax: # starts a comment until the end of the line
    ;; and <# #> denotes a multi-line comment
    (modify-syntax-entry ?\< "(> 1nb" st)
    (modify-syntax-entry ?\# "_ 123b" st)
    (modify-syntax-entry ?\> ")< 4nb" st)
    (modify-syntax-entry ?\n ">" st)
        
    ;; String syntax: " denotes a string
    (modify-syntax-entry ?\" "\"" st)

    ;; String syntax: ' denotes a string
    (modify-syntax-entry ?\' "\"" st)

    ;; Variable syntax: $ denotes a variable
    (modify-syntax-entry ?$ "w" st)

    ;; Brace syntax: (), [], and {} denote braces
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\{ "(}" st)

    st)
  "Syntax table for `koopa-mode'.")

;; Define the font-lock keywords
(defconst koopa-mode-font-lock-keywords
  ;; Highlight comments starting with #
  '(("\\(#.*\\)" 1 font-lock-comment-face)
    ;; Highlight documentation keywords
    ("\\.\\(DESCRIPTION\\|EXAMPLE\\|INPUTS\\|LINK\\|NOTES\\|OUTPUTS\\|PARAMETER\\|SYNOPSIS\\)" 0 font-lock-doc-face t)
    ;; Highlight variables that start with a $
    ("\\$\\(\\$\\|\\?\\|\\^\\|{?[a-zA-Z_][a-zA-Z0-9_]*}?\\)" 0 font-lock-variable-name-face t)
    ;; Highlight objects from the .NET framework
    ("\\[[a-zA-Z0-9_\\.]+\\]:\\{2\\}[a-zA-Z0-9_\\.]+" . font-lock-builtin-face)
    ;; Highlight control flow keywords
    ("\\<\\(for\\|if\\|else\\|elseif\\|switch\\|foreach\\|while\\|do\\)\\>" . font-lock-keyword-face)
    ;; Highlight loop control keywords
    ("\\<\\(break\\|continue\\)\\>" . font-lock-keyword-face)
    ;; Highlight script block keywords
    ("\\<\\(function\\|param\\|return\\)\\>" 0 font-lock-keyword-face t)
    ;; Highlight miscellaneous keywords
    ("\\<\\(begin\\|process\\|end\\)\\>" . font-lock-keyword-face)
    ;; Highlight PowerShell cmdlets
    ("\\<[a-zA-Z]+\\-[a-zA-Z]+" . font-lock-function-name-face)
    ;; Highlight command line arguments
    ("\s\\{1\\}--?[a-zA-Z0-9-]+" . font-lock-comment-face)
    ;; Highlight special characters
    ("`\\(0\\|a\\|b\\|e\\|f\\|n\\|r\\|t\\|u{\\w+}\\|v\\)" 0 font-lock-builtin-face t))
  "Keyword highlighting specification for `koopa-mode`.")

;; Define the indentation offset for PowerShell
(defcustom koopa-indent-offset 4
  "The indentation offset for PowerShell."
  :type 'natnum
  :safe #'natnump
  :group 'koopa)

;; Create a variable for the PowerShell executable
(defcustom koopa-powershell-executable
  (if (executable-find "pwsh") "pwsh"
                     "powershell")
  "The name of the system's PowerShell executable."
  :type 'string
  :group 'koopa)

;; Try to determine the operating system on which PowerShell is being run
;; This is used to spawn a dumb terminal for *nix OSes
(defvar koopa-is-running-on-windows
  (if (executable-find "powershell")
    t
    nil)
  "Indicates whether `koopa-mode' is being run on Windows.")

;; Define the PowerShell command line arguments
(defvar koopa-powershell-cli-arguments '("-NoProfile")
  "Arguments passed to the PowerShell executable.")

;; Define the PowerShell inferior shell buffer
(defvar koopa-powershell-buffer-name "*PowerShell*"
  "The name of the PowerShell buffer.")

;; Define the built-in PowerShell cmdlets
(defcustom koopa-powershell-cmdlets
  (let* ((cmd (format "%s -NoProfile -c \"Get-Command -CommandType Cmdlet | Select -Property Name | Format-Table -HideTableHeaders\"" koopa-powershell-executable))
	 (cmdlets (split-string (shell-command-to-string cmd) "\n" t)))
    ;; Remove any whitespace from the cmdlets
    (mapcar 'string-trim cmdlets))
  "All of the built-in cmdlets for PowerShell."
  :type 'list
  :group 'koopa)

;; Define the built-in PowerShell variables
(defcustom koopa-powershell-variables
  (let* ((cmd (format "%s -NoProfile -c \"Get-Variable | Select -Property Name | Format-Table -HideTableHeaders\"" koopa-powershell-executable))
       (variables (split-string (shell-command-to-string cmd) "\n" t))
       (prefixed-variables (mapcar (lambda (var) (concat "$" var)) variables)))
   (mapcar 'string-trim prefixed-variables))
  "All of the built-in variables for PowerShell."
  :type 'list
  :group 'koopa)

;; Define the user-defined PowerShell cmdlets
(defcustom koopa-custom-powershell-cmdlets '()
  "User-definedPowerShell cmdlets."
  :type 'list
  :group 'koopa)

;; Define the user-defined PowerShell variables
(defcustom koopa-custom-powershell-variables '()
  "User-defined PowerShell variables."
  :type 'list
  :group 'koopa)

;; Define the built-in .NET types
(defcustom koopa-powershell-dotnet-types
  (let* ((cmd (format "%s -NoProfile -c \"Get-TypeData | Select-Object -Property TypeName | Format-Table -HideTableHeaders\"" koopa-powershell-executable))
         (dotnet-types (split-string (shell-command-to-string cmd) "\n" t))
         ;; Remove any whitespace from the dotnet types
         (trimmed-dotnet-types (mapcar 'string-trim dotnet-types))
         (bracketed-dotnet-types (mapcar (lambda (dotnet-type) (concat dotnet-type "]")) trimmed-dotnet-types)))
    bracketed-dotnet-types)
  "All of the built-in .NET types for PowerShell."
  :type 'list
  :group 'koopa)

;; Define member methods
(defcustom koopa-powershell-member-methods '()
  "Member methods for PowerShell objects."
  :type 'list
  :group 'koopa)

;; Define .NET methods
(defcustom koopa-powershell-dotnet-methods '()
  "Methods for .NET types."
  :type 'list
  :group 'koopa)

;; Define the current line in `koopa-mode'
(defvar koopa-current-line-number (line-number-at-pos)
  "The current line number in `koopa-mode'.")

;; Manually indent a line
(defun koopa-indent-line ()
  "Manually indent a line by `koopa-indent-offset'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (indent-line-to (+ (current-indentation) koopa-indent-offset))))

;; Manually dedent a line
(defun koopa-dedent-line ()
  "Manually dedent a line by `koopa-indent-offset'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    ;; Ensure we don't end up dedenting to a negative value
    (indent-line-to (max 0 (- (current-indentation) koopa-indent-offset)))))

;; Automatically adjust indentation for a line
(defun koopa-auto-indent ()
  "Automatically indent the current line."
  (interactive)
  (let ((pos (point))
        (indent-level 0))
    ;; Save the cursor position
    (save-excursion
      ;; Check for closing braces on the current line
	(beginning-of-line)
	(if (looking-at "^[^\n{(\\[]*\\(}\\|)\\|\\]\\)$")
	    (setq indent-level (1- indent-level)))
      ;; As long as we are not at the beginning of the buffer, keep checking lines
      (while (not (bobp))
	;; Check the indentation on the previous line
        (forward-line -1)
        (beginning-of-line)
	;; Check for matching open/close characters
        (cond
	 ;; If there are closing characters, decrease the indentation level
         ((looking-at "^[^\n{(\\[]*\\(}\\|)\\|\\]\\)$")
	  (setq indent-level (1- indent-level)))
	  ;; If there are opening characters, increase the indentation level
         ((looking-at ".*\\({\\|(\\|\\[\\)$")
	  (setq indent-level (1+ indent-level)))
	 ;; If there are no opening or closing characters, do nothing
	 (nil))))
      ;; If the indent level is negative, set it to 0
      (if (< indent-level 0)
	  (setq indent-level 0))
      ;; Multiply the indent level by the `koopa-indent-offset' and indent the line
      (indent-line-to (* indent-level koopa-indent-offset))
  ;; Return to the initial position
  (goto-char pos)))

;; Define a function to insert a new line and adjust indentation
(defun koopa-newline-and-indent ()
  "Insert a newline and adjust PowerShell indentation."
  (interactive)
  (koopa-auto-indent)
  (end-of-line)
  (newline))

;; Create the PowerShell process
(defun koopa-run-powershell ()
  "Run an inferior instance of PowerShell in a new buffer."
  (interactive)
  ;; Create some local variables for the PowerShell program, buffer name, etc.
  (let* ((powershell-program koopa-powershell-executable)
	 (buffer (get-buffer-create koopa-powershell-buffer-name))
	 (process-connection-type nil)
	 (process (get-buffer-process buffer))
	 (default-directory default-directory))
    ; If we are using *nix, create a dumb terminal to handle escape sequences
    (unless koopa-is-running-on-windows
      (message "*nix OS detected, using dumb terminal")
      (setq process-environment (cons "TERM=dumb" process-environment)))
    ;; If the process is dead, reset the mode and restart the process
    (unless (get-buffer-process buffer)
      (with-current-buffer buffer
	(erase-buffer)
	(apply 'start-process "PowerShell" buffer powershell-program koopa-powershell-cli-arguments)
        (koopa-mode)
	(comint-mode)))
    ;; If there is a valid buffer, open it
    (when buffer
      (display-buffer-at-bottom buffer '((inhibit-same-window . t))))))

;; This sends a line to the PowerShell process spawned from `koopa-run-powershell'
(defun koopa-send-line-to-powershell ()
  "Send the current line to the *PowerShell* buffer."
  (interactive)
  (let ((line (thing-at-point 'line t))
        (buffer (get-buffer-create koopa-powershell-buffer-name))
	(process (get-buffer-process (get-buffer-create koopa-powershell-buffer-name))))
    ;; When the PowerShell process is alive, and the line is not blank, send it to the *PowerShell* buffer
    (when (and process (process-live-p process))
      (when (and line (not (string-blank-p line)))
	(comint-send-string (get-buffer-process buffer) line)))
    ;; If the PowerShell process is not started, notify the user
    (unless process
      (message "PowerShell process is not running. Use `C-c C-p` or `M-x koopa-run-powershell` to start a PowerShell process."))))

;; This sends the entire buffer to the PowerShell process spawned from `koopa-run-powershell'
(defun koopa-send-buffer-to-powershell ()
  "Send the entire buffer to the *PowerShell* buffer."
  (interactive)
  (let ((buffer (get-buffer-create koopa-powershell-buffer-name))
        (process (get-buffer-process (get-buffer-create koopa-powershell-buffer-name))))
    ;; When the PowerShell process is alive, send the current buffer to the *PowerShell* buffer
    (when (and process (process-live-p process))
      (with-current-buffer (current-buffer)
        (comint-send-region process (point-min) (point-max)))
      (comint-send-string process "\n"))
    ;; If the PowerShell process is not started, notify the user
    (unless process
      (message "PowerShell process is not running. Use `C-c C-p` or `M-x koopa-run-powershell` to start a PowerShell process."))))

;; Get the member methods for an object
(defun koopa-powershell-get-member-methods ()
  "Get the member methods from a PowerShell object."
  (save-excursion
  ;; Go to the previous line
  (forward-line -1)
  ;; Check for a variable assignment
  (if (re-search-forward "\\($[a-zA-Z0-9_]+\\)\s*=\s*[^\n;]+" nil t)
      (let* ((assignment (substring-no-properties (match-string 0)))
	     ;; If we are on Windows, we need to change double quotes to single quotes
	     (modified-assignment (if koopa-is-running-on-windows
				      (replace-regexp-in-string "\"" "'" assignment)
				    assignment))
	     (variable (substring-no-properties (match-string 1)))
	     ;; The command syntax differs between operating systems
	     (cmd (if koopa-is-running-on-windows
		      (format "%s -NoProfile -c \"%s; %s | Get-Member -ErrorAction SilentlyContinue | Select-Object -Property Name | Format-Table -HideTableHeaders\"" koopa-powershell-executable modified-assignment variable)
		    (format "%s -NoProfile -c '%s; %s | Get-Member -ErrorAction SilentlyContinue | Select-Object -Property Name | Format-Table -HideTableHeaders'" koopa-powershell-executable modified-assignment variable)))
	     ;; Join the object and its methods
	     (methods (split-string (shell-command-to-string cmd) "\n" t))
	     ;; Remove the whitespace from the member methods
	     (trimmed-methods (mapcar 'string-trim methods))
	     (member-methods (mapcar (lambda (method) (concat variable "." method "()")) trimmed-methods)))
	(setq koopa-powershell-member-methods (append koopa-powershell-member-methods member-methods))))))

;; Get user-defined cmdlets
(defun koopa-extract-custom-cmdlets-from-buffer ()
  "Extract user-defined cmdlets from the buffer."
    (save-excursion
      (goto-char (point-min))
      (let (cmdlets)
        (while (re-search-forward "\\(Function\\|function\\)\s+\\([a-zA-Z0-9-]+\\)" nil t)
          (push  (substring-no-properties (match-string 2)) cmdlets))
        (setq koopa-custom-powershell-cmdlets cmdlets))))

;; Get user-defined variables
(defun koopa-extract-custom-variables-from-buffer ()
  "Extract user-defined variables from the buffer."
    (save-excursion
      (goto-char (point-min))
      (let (variables)
        (while (re-search-forward "\\(\\$[a-zA-Z0-9_]+\\)\s*=\s*[^=].*" nil t)
          (push  (substring-no-properties (match-string 1)) variables))
        (setq koopa-custom-powershell-variables variables))))

;; Remove unnecessary member variables
(defun koopa-cleanup-powershell-member-methods ()
  "Remove deleted variables from `koopa-powershell-get-member-methods'."
  (setq koopa-powershell-member-methods
	(let (methods-still-in-use '())
	  (dolist (method koopa-powershell-member-methods)
	    ;; Extract the variable from the method
	    (let ((variable (car (split-string method "\\."))))
	      ;; Don't add unused or null values to the list
	      (unless (or
		       (not (member variable koopa-custom-powershell-variables))
		       (eq variable nil))
		(add-to-list 'methods-still-in-use method))))
	  methods-still-in-use)))

;; Add methods to `koopa-powershell-dotnet-methods'
(defun koopa-update-powershell-dotnet-methods ()
  "Update `koopa-powershell-dotnet-methods' with the current .NET type's methods."
  ;; Extract the member from the current line
  (save-excursion
    (when (re-search-backward "^\\(\\[[a-zA-Z0-9\.]+\\]\\)::$" nil t)
      (let* ((dotnet-type (substring-no-properties (match-string 1)))
	     (cmd (format "%s -NoProfile -c \"%s.GetMethods() | Select-Object -ExpandProperty Name\"" koopa-powershell-executable dotnet-type))
	     (methods (split-string (shell-command-to-string cmd) "\n" t))
	     ;; Remove any whitespace from the methods
	     (trimmed-methods (mapcar 'string-trim methods)))
	;; Update `koopa-powershell-dotnet-methods
	(setq koopa-powershell-dotnet-methods trimmed-methods)))))
	     
;; Monitor the buffer for user-defined cmdlets and variables
(defun koopa-monitor-code-changes ()
  "Monitor code and update custom cmdlets and variables lists."
  (when (and (eq major-mode 'koopa-mode)
             (buffer-modified-p))
    (koopa-extract-custom-cmdlets-from-buffer)
    (koopa-extract-custom-variables-from-buffer)))

;; Create the company backend
(defun koopa-company-backend (command &optional arg &rest _ignored)
    "Company completion backend for `koopa-mode'.

This function provides `company-mode' completions for `koopa-mode'
by implementingvarious COMMANDs.  COMMAND specifies the
action to be performed by the backend.

COMMAND options:
- 'interactive': Activate the `koopa-company-backend' as the active backend.
- 'prefix': Generate a prefix for company completions based on the current
  point in the buffer and the major mode.
- 'candidates': Return a list of completion CANDIDATES based on the current
  buffer context.
- 'duplicates': Indicates whether the backend supports handling duplicate
  candidates.

The backend offers different types of completions:
- For .NET types in square brackets (e.g., '[System.String]'), suggests
  relevant .NET types.
- For .NET type methods (e.g., '[System.String]::Substring'), suggests
  appropriate method completions.
- Default completions include PowerShell cmdlets, variables, and custom
  cmdlets and variables.

COMMAND, ARG, and _IGNORED are arguments passed by `company-mode' and should
not be provided manually."
  (interactive (list 'interactive))
  (cond
    ((eq command 'interactive) (company-begin-backend 'koopa-company-backend))
    ((eq command 'prefix) (and
			   (eq major-mode 'koopa-mode)
			   (company-grab-symbol)))
    ((eq command 'candidates)
     (let ((completion-candidates
	    (cond
	     ;; Company completions for .NET types
	     ((looking-back "\\[[^:]*" (line-beginning-position)) koopa-powershell-dotnet-types)
	     ;; Company completions for .NET type methods
	     ((looking-back "::.*" (line-beginning-position)) koopa-powershell-dotnet-methods)
	     ;; Default completions
	     ((append
               koopa-powershell-cmdlets
               koopa-powershell-variables
               koopa-custom-powershell-cmdlets
               koopa-custom-powershell-variables
	       koopa-powershell-member-methods)))))
       (all-completions arg completion-candidates)))
    ((eq command 'duplicates) t)))

;; Define a function to trigger company completion manually
(defun koopa-trigger-company-complete ()
  "Invoke company completion for PowerShell cmdlets."
  (interactive)
  (company-complete))

;; Create a function to update `newline-hook'
(defun koopa-update-newline-hook ()
  "Check if a newline was inserted."
  (let ((new-line-number (line-number-at-pos))
	(key (this-command-keys)))
    (when (and
	   (not (equal new-line-number koopa-current-line-number))
	   (equal key (kbd "RET")))
      (setq koopa-current-line-number new-line-number)
      (run-hooks 'koopa-newline-hook))))

;; Create a function to update `koopa-static-member-hook'
(defun koopa-update-static-member-hook ()
  "Check if a .NET type is being invoked."
  (when (looking-back "::" (line-beginning-position)) (run-hooks 'koopa-static-member-hook)))


(provide 'koopa-mode)
;;; koopa-mode.el ends here
