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
  (local-set-key (kbd "C-c C-p") 'run-powershell)
  )

;; Define the syntax table
(defvar koopa-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comment syntax: # starts a comment until the end of the line
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
   
    ;; String syntax: " denotes a string
    (modify-syntax-entry ?\" "\"" st)

    ;; Variable syntax: $ denotes a variable
    (modify-syntax-entry ?$ "w" st)

    ;; Brace syntax: (), [], and {} denote braces
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\{ "(}" st)

    st)
  "Syntax table for `koopa-mode`.")

; Ignore case with font-lock regular expressions
; (setq-local font-lock-keywords-case-fold-search t)

;; Define the font-lock keywords
(defvar koopa-mode-font-lock-keywords
  ; Highlight comments starting with #
  '(("\\(#.*\\)" 1 font-lock-comment-face)
    ; Highlight multi-line comments
    ; ("<#[^#]*[^>]*#>" font-lock-comment-face)
    ; Highlight variables that start with a $
    ("\\<\\$[a-zA-Z_][a-zA-Z0-9_]*\\>" . font-lock-variable-name-face)
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


;; Define the PowerShell inferior shell buffer
(defvar powershell-buffer-name "*PowerShell*")

(defun run-powershell ()
  "Run PowerShell in an inferior shell buffer."
  (interactive)
  (let ((powershell-program "powershell.exe"))
    (pop-to-buffer (get-buffer-create powershell-buffer-name))
    (unless (comint-check-proc powershell-buffer-name)
      (apply 'make-comint-in-buffer "PowerShell" powershell-buffer-name powershell-program nil '("-NoExit" "-Command" "-")))
    (setq-local company-backends '(company-capf))))

;; Set up PowerShell buffer for auto-completion in koopa-mode
(add-hook 'koopa-mode-hook (lambda () (setq-local company-backends '(company-files company-capf))))

(provide 'koopa-mode)
;;; koopa-mode.el ends here
