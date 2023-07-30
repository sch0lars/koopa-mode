;;; koopa-mode.el --- A major mode for PowerShell

;;; Code:
;; Define koopa-mode
(define-derived-mode koopa-mode prog-mode "A major mode for Microsoft PowerShell"
  "A major mode for editing PowerShell scripts."
  ;; Set the syntax table
  (setq-local syntax-table koopa-mode-syntax-table)
  ;; Set the font-lock keywords
  (setq-local font-lock-defaults '(koopa-mode-font-lock-keywords))
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

; Ignore case for regular expressions
(setq-default case-fold-search t)

;; Define the font-lock keywords
(defvar koopa-mode-font-lock-keywords
  ; Highlight comments starting with #
  '(("\\(#.*\\)" 1 font-lock-comment-face)
    ; Highlight variables starting with $
    ("\\($[a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-variable-name-face)
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

;; Add any keybindings or other customizations here as needed
;; For example, defining indentation rules, keybindings for common commands, etc.

(provide 'koopa-mode)
;;; koopa-mode.el ends here
