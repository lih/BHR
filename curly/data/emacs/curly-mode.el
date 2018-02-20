(defconst curly-default-offset 2)
(defvar curly-font-lock-head nil)

(defun curly-find-arguments-end ()
  (while (and (search-forward-regexp "\\([{}]\\|[:=]\\>\\)" nil t)
              (when (looking-back "{") (backward-char) (forward-sexp) t))))
(defun curly-looking-at-let-args ()
  (save-excursion (curly-find-arguments-end) (looking-back "=")))

(defun curly-find-argument (lim)
  (condition-case nil
      (progn
	(when (looking-at "[:=]\\>")
	  (backward-up-list) (forward-sexp))
	(condition-case nil
	    (while (and (search-forward-regexp "\\sw\\|[{(]")
			(cond
			 ((looking-back "{") (let ((prev (save-excursion (curly-find-arguments-end) (char-before))))
					       (cond
						((eq prev ?}) (backward-char) (forward-sexp) t)
						(t (prog1 t (setq curly-font-lock-head (eq prev ?=)))))))
			 ((looking-back "(") (prog1 t (backward-char) (forward-sexp))))))
	  (search-error nil))
	(when (and (looking-back "\\sw") (>= lim (point)))
	  (backward-char)
	  (set-match-data (let ((beg (point))
				(end (progn (forward-word)
					    (when (looking-back "[:=]") (backward-char))
					    (point))))
			    (if curly-font-lock-head (list beg end nil nil beg end)
			      (list beg end beg end nil nil))))
	  (setq curly-font-lock-head nil)
	  t))
    (scan-error nil)))

(defun curly-partial-tail (forms)
  (cond
   ((null forms) "")
   (t (let ((x (car forms)) (y (cdr forms)))
        (concat "\\(?:\\s-+\\(" (car forms) "\\)" (curly-partial-tail (cdr forms)) "\\)?")
        ))))

(defconst curly-mode-keywords
  (list
   (list "^\\(import\\|export\\|transport\\)\\(s?\\)\\>"
	 '(0 font-lock-builtin-face)
	 '("\\(\\sw+\\){" nil nil (1 font-lock-type-face)))
   (list "^\\(multi\\)\\s-+\\(\\sw+\\)\\s-*\\(=\\)\\s-+\\(\\sw*[^,[:blank:]]\\)"
	 '(1 font-lock-builtin-face) '(2 font-lock-function-name-face)
	 '(3 font-lock-builtin-face) '(4 font-lock-function-name-face)
	 '("\\(,\\)\\s-*\\(\\sw+\\)\\s-+\\(\\sw*[^,[:blank:]]\\)" nil nil
	   (1 font-lock-keyword-face) (2 font-lock-keyword-face) (3 font-lock-function-name-face)
	   )
	 )
   (list "^\\(describe\\)\\s-+\\(\\sw+\\)\\s-*\\(as\\)"
         '(1 font-lock-builtin-face) '(2 font-lock-function-name-face) '(3 font-lock-builtin-face)
         '("." (save-excursion
                 (while (progn
                          (forward-sexp)
                          (skip-chars-forward " \t")
                          (not (or (looking-at "\n") (eq (point) (point-max))))))
                 (point))
           nil
           (0 font-lock-doc-face)))
   (list
    (setq val (concat "\\(?:{\\|^\\)\\(type\\>\\)\\(?:\\s-+\\(\\sw+\\)\\s-+\\(:\\)\\)?"
	    (curly-partial-tail '("\\sw+\\)\\(\\(?:\\s-+[^=[:blank:]]\\sw*\\)*"
				  "="      "\\sw+"       ":"))))
    
    '(1 font-lock-builtin-face t t)
    
    '(2 font-lock-function-name-face t t) '(3 font-lock-builtin-face)
    '(4 font-lock-type-face t t) '(5 font-lock-variable-name-face t t) '(6 font-lock-builtin-face t t)
    '(7 font-lock-function-name-face t t) '(8 font-lock-builtin-face t t)

    '("\\<or\\>" nil nil (0 font-lock-builtin-face t t)))
   (list
    (concat "^\\(family\\)" (curly-partial-tail '("\\sw+" "\\(?:[^:]\\sw*\\(?:\\s-+[^:]\\sw*\\)*\\)?" ":")))
    '(1 font-lock-builtin-face) '(2 font-lock-type-face) '(3 font-lock-variable-name-face)
    '(4 font-lock-builtin-face))

   (list (concat "^\\(module\\)\\s-+\\(\\(?:[^ \t\n:]\\sw*\\s-+\\)+:\\)?\\([^\n]*\\)$")
         '(1 font-lock-builtin-face t t) '(2 font-lock-builtin-face t t) '(3 font-lock-doc-face t t))

   (list "^\\(define\\|operator\\|function\\|let\\)\\s-+\\(\\sw+\\)"
         '(1 font-lock-builtin-face) '(2 font-lock-function-name-face)
         '(curly-find-argument (save-excursion (curly-find-arguments-end) (point)) nil
                               (1 font-lock-variable-name-face nil t)
			       (2 font-lock-function-name-face nil t)))


   (list "\\(\"[^\"]*\"\\)" 0 font-lock-string-face)
   (list "[{}]" 0 font-lock-function-name-face)
   (list "_" 0 font-lock-builtin-face)
   (cons "[()]" font-lock-builtin-face)
   (list "\\<=\\>" 0 font-lock-builtin-face t)
   (list "[0-9][0-9]*" 0 font-lock-constant-face t)
   (cons (lambda (lim)
           (while (and
                   (< (point) lim)
                   (or (memq (get-char-code-property (char-after) 'general-category)
                             '(Ll Lu Lo Lt Lm Mn Mc Me Nl))
                       (memq (char-syntax (char-after)) '(?\( ?\) ? ))
                       (and (>= (char-after) ?0) (<= (char-after) ?9))))
             (forward-char))
           (when (< (point) lim) (prog1 t
                                   (set-match-data (list (point) (1+ (point)))))))
         '(0 font-lock-constant-face))
   
   (list "{"
         '(curly-find-argument
	   (progn (backward-char)
		  (add-text-properties (point) (save-excursion (forward-sexp) (point)) '(font-lock-multiline t))
		  (max (save-excursion (end-of-line) (point))
		       (save-excursion (forward-sexp) (point))))
	   nil
	   (1 font-lock-variable-name-face)
	   (2 font-lock-function-name-face t t)))

   (list "^\\s-*#.*$" 0 font-lock-comment-face t)))

(defconst curly-mode-syntax-table
  (with-syntax-table (make-char-table 'syntax-table)
    (let ((set-syntax-list (lambda (es v) (mapc (lambda (e) (modify-syntax-entry e v)) es)) ))
      (funcall set-syntax-list '((0 . 255)) "w")
      (funcall set-syntax-list '((?a . ?z) (?A . ?Z) (?0 . ?9) ?\') "w")
      (modify-syntax-entry ?\( "()")
      (modify-syntax-entry ?\{ "(}")
      (funcall set-syntax-list '(?\  ?\t ?\n) " ")
      (funcall set-syntax-list '(?\) ?}) ")")
      (funcall set-syntax-list '(?\\) "\\")
      (funcall set-syntax-list '(?\") "\"")
      (funcall set-syntax-list '(?#) "w<14")
      (funcall set-syntax-list '(? ) " >23")
      (syntax-table))))

(defun curly-interactive-session ()
  (interactive)
  (let ((path (reverse (split-string (buffer-file-name) "/")))
        exec)
    (while (not (null path))
      (setq exec (mapconcat 'identity (reverse (cons "curly" path)) "/"))
      (if (file-exists-p exec) (setq path nil) (setq path (cdr path))))
    (let ((bname (concat "*curly:" exec "*")))
      (switch-to-buffer (get-buffer-create bname))
      (term-ansi-make-term bname exec nil "--interactive")
      (term-mode)
      (term-char-mode)
      (term-set-escape-char ?\C-x))))
(defun curly-current-indent ()
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (skip-chars-forward " \t")
      (- (point) start))))
(defun curly-indent-line ()
  (let ((inhibit-point-motion-hooks t)
        (indent-here (curly-current-indent))
        start start-list start-line end (depth 0) (inc 2))
    (when (save-excursion (beginning-of-line) (setq start (point)) (looking-at "\\s-*\\s)"))
      (setq inc 0))
    (save-excursion
      (indent-line-to
       (condition-case nil
           (save-excursion
             (setq start-list (condition-case nil
                                  (save-excursion (while (>= (point) start) (backward-up-list) (setq depth (1+ depth)))
                                                  (beginning-of-line) (point))
                                (scan-error 0)))
             (setq start-line (save-excursion (forward-line -1) (beginning-of-line) (point)))
             (goto-char (max start-line start-list))
             (skip-chars-forward " \t")
             (setq end (point))
             (max indent-here (if (and (> start-line start-list) (not (looking-at "}")))
                                  (- end start-line)
                                (+ (- end start-list) (* depth inc)))))
         (scan-error 0))))
    (when (looking-back "^[ \t]*") (skip-chars-forward " \t"))))

(defun curly-post-self-insert-function ()
  (when (looking-back "\\.\\.\\.")
    (delete-region (- (point) 3) (point))
    (insert-char ?…)))

;;;###autoload
(define-derived-mode curly-mode lisp-mode "Curly Calculus"
  "Major mode for editing Curly source files.\n\n\\{curly-mode-map}"
  :syntax-table curly-mode-syntax-table
  (run-mode-hooks 'curly-mode-hook)
  (set (make-local-variable 'font-lock-defaults) '(curly-mode-keywords t))
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'indent-line-function) 'curly-indent-line)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "#")
  (set (make-local-variable 'comment-style) 'plain)
  (add-hook (make-local-variable 'post-self-insert-hook) 'curly-post-self-insert-function)
  (local-set-key (kbd "C-c C-i") 'curly-interactive-session))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.c\\(url\\)?y\\'" . curly-mode))

(provide 'curly-mode)
