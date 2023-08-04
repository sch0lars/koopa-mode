;;; koopa-mode.el --- A major mode for Microsoft PowerShell

;;; Code:
;; Define koopa-mode
(define-derived-mode koopa-mode prog-mode "koopa-mode"
  "A major mode for editing Microsoft PowerShell scripts."
  ;; Set the syntax table
  (setq-local syntax-table koopa-mode-syntax-table)
  ;; Set the font-lock keywords
  (setq-local font-lock-defaults '(koopa-mode-font-lock-keywords))
  ; Make font-lock keywords case-insensitive
  (set (make-local-variable 'font-lock-defaults)
       '(koopa-mode-font-lock-keywords nil t))
  ;; Set company-backends for PowerShell buffer
  (setq-local company-backends '(company-files company-capf))
  ;;; Keybindings
  ;; Bind run-powershell to C-c C-p
  (local-set-key (kbd "C-c C-p") 'koopa-run-powershell)
  (local-set-key (kbd "C-c C-c") 'koopa-send-line-to-powershell)
  (local-set-key (kbd "C-c C-b") 'koopa-send-buffer-to-powershell))

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
  ; Highlight comments starting with #
  '(("\\(#.*\\)" 1 font-lock-comment-face)
    ; Highlight variables that start with a $
    ; ("\\${?[a-zA-Z_][a-zA-Z0-9_]*}?" . font-lock-variable-name-face)
    ("\\${?[a-zA-Z_][a-zA-Z0-9_]*}?" 0 font-lock-variable-name-face t)
    ; Highlight objects from the DotNet framework
    ("\\[[a-zA-Z0-9_\\.]+\\]:\\{2\\}[a-zA-Z0-9_\\.]+" . font-lock-builtin-face)
    ; Highlight control flow keywords
    ("\\<\\(for\\|if\\|else\\|elseif\\|switch\\|foreach\\|while\\|do\\)\\>" . font-lock-keyword-face)
    ; Highlight loop control keywords
    ("\\<\\(break\\|continue\\)\\>" . font-lock-keyword-face)
    ; Highlight script block keywords
    ("\\<\\(function\\|param\\|return\\)\\>" . font-lock-keyword-face)
    ; Highlight miscellaneous keywords
    ("\\<\\(begin\\|process\\|end\\)\\>" . font-lock-keyword-face)
    ; Highlight PowerShell cmdlets
    ("\\<[a-zA-Z]+\\-[a-zA-Z]+" . font-lock-function-name-face))
  "Keyword highlighting specification for `koopa-mode`.")

;; For example, defining indentation rules, keybindings for common commands, etc.


;; Create a variable for the PowerShell executable
(defcustom koopa-powershell-executable
  (if (executable-find "pwsh") "pwsh"
                     "powershell")
  "The name of the system's PowerShell executable."
  :type 'string
  :group 'koopa-mode)

;; Define the PowerShell command line arguments
(defvar koopa-powershell-cli-arguments '("-NoProfile")
  "Arguments passed to the PowerShell executable.")

;; Define the PowerShell inferior shell buffer
(defvar koopa-powershell-buffer-name "*PowerShell*"
  "The name of the PowerShell buffer.")


;; Create the PowerShell process
(defun koopa-run-powershell ()
  "Run an inferior instance of PowerShell in a new buffer."
  (interactive)
  (let* ((powershell-program koopa-powershell-executable)
	 (buffer (get-buffer-create koopa-powershell-buffer-name))
	 (process-connection-type nil)
	 (process (get-buffer-process buffer))
	 (default-directory default-directory)
         (process-environment (cons "TERM=dumb" process-environment)))
    ; If the process is dead, reset the mode and restart the process
    (unless (get-buffer-process buffer)
      (with-current-buffer buffer
	(erase-buffer)
	(apply 'start-process "PowerShell" buffer powershell-program koopa-powershell-cli-arguments)
        (koopa-mode)
	(comint-mode)))
    ; If there is a valid buffer, pop to it
    (when buffer
      (display-buffer-at-bottom buffer '((inhibit-same-window . t))))))


(defun koopa-send-line-to-powershell ()
  "Send the current line to the *PowerShell* buffer."
  (interactive)
  (let ((line (thing-at-point 'line t))
        (buffer (get-buffer-create koopa-powershell-buffer-name))
	(process (get-buffer-process (get-buffer-create koopa-powershell-buffer-name))))
    (when (and process (process-live-p process))
      (when (and line (not (string-blank-p line)))
	(comint-send-string (get-buffer-process buffer) line)))
    (unless process
      (message "PowerShell process is not running. Use `C-c C-p` or `M-x koopa-run-powershell` to start a PowerShell process."))))


(defun koopa-send-buffer-to-powershell ()
  "Send the entire buffer to the *PowerShell* buffer."
  (interactive)
  (let ((buffer (get-buffer-create koopa-powershell-buffer-name))
        (process (get-buffer-process (get-buffer-create koopa-powershell-buffer-name))))
    (when (and process (process-live-p process))
      (with-current-buffer (current-buffer)
        (comint-send-region process (point-min) (point-max)))
      (comint-send-string process "\n"))
    (unless process
      (message "PowerShell process is not running. Use `C-c C-p` or `M-x koopa-run-powershell` to start a PowerShell process."))))


(provide 'koopa-mode)
;;; koopa-mode.el ends here
