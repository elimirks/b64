(require 'generic-x)

(define-generic-mode 'b-mode
  '(("/*" . "*/"))
  '("return" "auto" "extern" "eof" "while" "if" "else" "goto" "switch" "break")
  '(("\\b[0-9]+\\b" . font-lock-constant-face))
  '("\\.b$")
  '() ;; TODO: Highlight string & char literals here
  "A mode for B files")

(add-hook 'b-mode-hook
          (lambda () (run-hooks 'prog-mode-hook)))

(provide 'b-mode)
