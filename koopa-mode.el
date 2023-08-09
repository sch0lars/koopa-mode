;;; koopa-mode.el --- A major mode for Microsoft PowerShell

(require 'cl-lib)
(require 'company)

;;; Code:
;; Define koopa-mode
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
  (local-set-key (kbd "C-c C-p") 'koopa-run-powershell)
  (local-set-key (kbd "C-c C-c") 'koopa-send-line-to-powershell)
  (local-set-key (kbd "C-c C-b") 'koopa-send-buffer-to-powershell)

  ;; Set up company-mode in koopa-mode
  (add-hook 'koopa-mode-hook
            (lambda ()
              (company-mode t)
              (setq-local company-backends '(koopa-company-backend)))))

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
  "Syntax table for `koopa-mode`.")

;; Define the font-lock keywords
(defconst koopa-mode-font-lock-keywords
  ;; Highlight comments starting with #
  '(("\\(#.*\\)" 1 font-lock-comment-face)
    ;; Highlight documentation keywords
    ("\\.\\(DESCRIPTION\\|EXAMPLE\\|INPUTS\\|LINK\\|NOTES\\|OUTPUTS\\|PARAMETER\\|SYNOPSIS\\)" 0 font-lock-doc-face t)
    ;; Highlight variables that start with a $
    ("\\${?[a-zA-Z_][a-zA-Z0-9_]*}?" 0 font-lock-variable-name-face t)
    ;; Highlight objects from the DotNet framework
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
  "Indicates whether koopa-mode is being run on Windows.")

;; Define the PowerShell command line arguments
(defvar koopa-powershell-cli-arguments '("-NoProfile")
  "Arguments passed to the PowerShell executable.")

;; Define the PowerShell inferior shell buffer
(defvar koopa-powershell-buffer-name "*PowerShell*"
  "The name of the PowerShell buffer.")

(defcustom koopa-powershell-cmdlets
  (let* ((cmd (format "%s -NoProfile -c \"Get-Command -CommandType Cmdlet | Select -Property Name | Format-Table -HideTableHeaders\"" koopa-powershell-executable))
	 (cmdlets (split-string (shell-command-to-string cmd) "\n" t)))
    ;; Remove any whitespace from the cmdlets
    (mapc 'string-trim cmdlets))
  "All of the built-in cmdlets for PowerShell."
  :type 'list
  :group 'koopa)

;; Manually indent a line
(defun koopa-indent-line ()
  "Manually indent a line by `koopa-indent-offset`."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (indent-line-to (+ (current-indentation) koopa-indent-offset))))

;; Manually dedent a line
(defun koopa-dedent-line ()
  "Manually dedent a line by `koopa-indent-offset`."
  (interactive)
  (save-excursion
    (beginning-of-line)
    ;; Ensure we don't end up dedenting to a negative value
    (indent-line-to (max 0 (- (current-indentation) koopa-indent-offset)))))

;; Automatically adjust indentation for a line
(defun koopa-auto-indent ()
  "Automatically indent current line according to PowerShell indentation conventions."
  (interactive)
  (let ((pos (point))
        (indent-level 0))
    ;; DEBUG
    ;; (message (format "DEBUG: Initial indent: %d" indent-level))
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
         ((looking-at ".*\\(}\\|)\\|\\]\\)$")
	  (setq indent-level (1- indent-level)))
	  ;; If there are opening characters, increase the indentation level
         ((looking-at ".*\\({\\|(\\|\\[\\)$")
	  (setq indent-level (1+ indent-level)))
	 ;; If there are no opening or closing characters, do nothing
	 nil)))
      ;; If the indent level is negative, set it to 0
      (if (< indent-level 0)
	  (setq indent-level 0))
      ;; Multiply the indent level by the `koopa-indent-offset` and indent the line
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
    ;; If there is a valid buffer, pop to it
    (when buffer
      (display-buffer-at-bottom buffer '((inhibit-same-window . t))))))

;; This sends a line to the PowerShell process spawned from `koopa-run-powershell`
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

;; This sends the entire buffer to the PowerShell process spawned from `koopa-run-powershell`
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
    ; If the PowerShell process is not started, notify the user
    (unless process
      (message "PowerShell process is not running. Use `C-c C-p` or `M-x koopa-run-powershell` to start a PowerShell process."))))

;; Get the PowerShell cmdlet suggestions for a prefix
(defun koopa-company-complete-powershell-cmdlets (arg)
  "Generate a list of PowerShell cmdlets as completion candidates."
  (let ((matching-cmdlets '()))
    (cl-loop for cmdlet in koopa-powershell-cmdlets
             when (string-prefix-p arg cmdlet)
             collect cmdlet into matching-cmdlets
             finally return matching-cmdlets)))


(defun koopa-company-backend (command &optional arg &rest _ignored)
  "Company backend for PowerShell cmdlet completion in koopa-mode."
  (interactive (list 'interactive))
  (cond
    ((eq command 'interactive) (company-begin-backend 'koopa-company-backend))
    ((eq command 'prefix) (and (eq major-mode 'koopa-mode) (company-grab-symbol)))
    ((eq command 'candidates) (koopa-company-complete-powershell-cmdlets arg))
    ((eq command 'duplicates) t)))

;; Define a function to trigger company completion manually
(defun koopa-trigger-company-complete ()
  "Invoke company completion for PowerShell cmdlets."
  (interactive)
  (company-complete))



(provide 'koopa-mode)
;;; koopa-mode.el ends here
