(defconst curly-conf-mode-keywords
  (list
   '("^\\(\\(\\+\\S-*\\s-+\\)*\\)\\(include\\)\\s-+\\([^=]*\\)="
     (1 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     (4 font-lock-variable-name-face))
   '("^\\(\\(\\+\\S-*\\s-+\\)*\\)\\(flag\\)\\s-+\\(.*\\)$"
     (1 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     (4 font-lock-function-name-face))
   '("^\\(\\(\\+\\S-*\\s-+\\)*\\)\\(>\\|echo\\)\\(.*\\)$"
     (1 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     (4 font-lock-string-face))
   '("^\\(\\?\\S-*\\)\\s-+\\(.*\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-doc-face))
   '("^\\(\\(\\+\\S-*\\s-+\\)*\\)\\(%.*\\)$"
     (1 font-lock-function-name-face)
     (3 font-lock-keyword-face))
   '("^\\(\\(\\+\\S-*\\s-+\\)*\\)\\(target\\|-\\)\\s-+\\(interactive\\|prelude\\+?\\|banner\\|instance\\|at\\|run\\|serve\\|translate\\|execute\\|list\\|dump\\)"
     (1 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     (4 font-lock-builtin-face))
   '("^\\(\\(\\+\\S-*\\s-+\\)*\\)\\(mount\\)\\s-+\\([^=]*\\)=\\s-*\\(\\(?:re\\)?source\\|library\\|builtins\\|package\\)"
     (1 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     (4 font-lock-variable-name-face)
     (5 font-lock-builtin-face))
   '("^#.*$" . font-lock-comment-face)
   '("^.*$" . font-lock-warning-face)))

;;;###autoload
(define-derived-mode curly-conf-mode fundamental-mode "Curly Context"
  "Major mode providing syntactic coloration to Curly configuration files."
  (run-mode-hooks 'curly-conf-mode-hook)
  (set (make-local-variable 'font-lock-defaults) '(curly-conf-mode-keywords t))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "#")
  (set (make-local-variable 'comment-style) 'plain))

;;;###autoload (add-to-list 'interpreter-mode-alist '("curly" . curly-conf-mode))
