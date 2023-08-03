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
  ;; Enable company-mode for auto-completion
  (company-mode 1)
  ;; Set company-backends for PowerShell buffer
  (setq-local company-backends '(company-files company-capf))

  ;; Keybindings
  ;; Bind run-powershell to C-c C-p
  (local-set-key (kbd "C-c C-p") 'koopa-run-powershell)
  )

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
    ("\\<[a-zA-Z]+\\-[a-zA-Z]+" . font-lock-function-name-face)
    ; Highlight member accessors
    (("\\(\\$[a-zA-Z0-9_]+\\.\\)\\([a-zA-Z0-9_]+\\)"  (2 font-lock-function-name-face nil t))))
  "Keyword highlighting specification for `koopa-mode`.")

;; For example, defining indentation rules, keybindings for common commands, etc.


;; Create a variable for the PowerShell executable
(defcustom koopa-powershell-executable "pwsh"
  "The name of the system's PowerShell executable."
  :type 'string
  :group 'koopa-mode)

(defvar koopa-powershell-cli-arguments '("-i")
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
	 (proc-alive (comint-check-proc buffer))
	 (process (get-buffer-process buffer)))
    ; If the process is dead, reset the mode and restart the process
    (unless proc-alive
      (with-current-buffer buffer
	(apply 'make-comint-in-buffer "PowerShell" buffer powershell-program nil koopa-powershell-cli-arguments)
	(koopa-mode)))
    ; If there is a valid buffer, pop to it
    (when buffer
      (pop-to-buffer buffer))))
    
;; Set up PowerShell buffer for auto-completion in koopa-mode
(add-hook 'koopa-mode-hook (lambda () (setq-local company-backends '(company-files company-capf))))

(provide 'koopa-mode)
;;; koopa-mode.el ends here
