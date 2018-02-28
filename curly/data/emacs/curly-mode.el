(require 'curly-utils)

(defconst curly-default-offset 2)
(defvar curly-font-lock-head nil)

(defun curly-find-arguments-end ()
  (while (and (search-forward-regexp "\\([{}]\\|[:=]\\>\\)" nil t)
              (when (looking-back "{" nil) (backward-char) (forward-sexp) t))))
(defun curly-looking-at-let-args ()
  (save-excursion (curly-find-arguments-end) (looking-back "=" nil)))

(defun curly-find-argument (lim)
  (condition-case nil
      (progn
	(when (looking-at "[:=]\\>")
	  (backward-up-list) (forward-sexp))
	(condition-case nil
	    (while (and (search-forward-regexp "\\sw\\|[{(]")
			(cond
			 ((looking-back "{" nil) (let ((prev (save-excursion (curly-find-arguments-end) (char-before))))
					       (cond
						((eq prev ?}) (backward-char) (forward-sexp) t)
						(t (prog1 t (setq curly-font-lock-head (eq prev ?=)))))))
			 ((looking-back "(" nil) (prog1 t (backward-char) (forward-sexp))))))
	  (search-error nil))
	(when (and (looking-back "\\sw" nil) (>= lim (point)))
	  (backward-char)
	  (set-match-data (let ((beg (point))
				(end (progn (forward-word)
					    (when (looking-back "[:=]" nil) (backward-char))
					    (point))))
			    (if curly-font-lock-head (list beg end nil nil beg end)
			      (list beg end beg end nil nil))))
	  (setq curly-font-lock-head nil)
	  t))
    (scan-error nil)))

(defconst curly-mode-keywords 
  (list
   '("\\(\"[^\"]*\"\\)" 0 font-lock-string-face)
   '("^\\s-*#.*$" 0 font-lock-comment-face)
   
   (curly-keyword (:bol (:capture (:or "import" "export" "transport") (:optional "s")) :eow)
     '(1 font-lock-builtin-face)
     '("\\(\\sw+\\){" nil nil (1 font-lock-type-face)))

   (curly-keyword (:bol (:partial (:capture "multi")
				  (:nbsp (:capture :word))
				  (:spc (:capture "="))
				  (:spc (:capture "\\sw*[^,[:blank:]]"))))
     '(1 font-lock-builtin-face) '(2 font-lock-function-name-face)
     '(3 font-lock-builtin-face) '(4 font-lock-function-name-face)

     (curly-keyword (:partial (:capture ",") (:spc (:capture :word)) (:nbsp (:capture "\\sw*[^,[:blank:]]")))
       nil nil
       '(1 font-lock-keyword-face) '(2 font-lock-keyword-face) '(3 font-lock-function-name-face)))
   
   (curly-keyword (:bol (:partial (:capture "describe")
				  (:nbsp (:capture :word))
				  (:nbsp (:capture "as" :eow))))
     '(1 font-lock-builtin-face) '(2 font-lock-function-name-face) '(3 font-lock-builtin-face)
     '("." (save-excursion
	     (while (progn
		      (forward-sexp)
		      (skip-chars-forward " \t")
		      (not (or (looking-at "\n") (eq (point) (point-max))))))
	     (point))
       nil
       (0 font-lock-doc-face)))
   (curly-keyword (:bol (:partial (:capture "type")
				  (:nbsp (:capture :word))
				  (:optional :spc (:capture ":") :nbsp (:capture :word))
				  (:capture (:many :nbsp :word))
				  (:nbsp (:capture "="))
				  (:nbsp (:capture :word))
				  (:capture (:many :nbsp :word))
				  (:nbsp (:capture ":"))))
     '(1 font-lock-builtin-face)
     '(2 font-lock-function-name-face)
     '(3 font-lock-builtin-face nil t) '(4 font-lock-type-face nil t)
     '(5 font-lock-variable-name-face)
     '(6 font-lock-builtin-face)
     '(7 font-lock-function-name-face)
     '(8 font-lock-variable-name-face)
     '(9 font-lock-builtin-face)

     '("\\<or\\>" nil nil (0 font-lock-builtin-face)))
   
   (curly-keyword (:bol (:partial (:capture "family")
				  (:nbsp (:capture "[^[[:blank:]]+"))
				  (:capture (:many "\\[[^]]*\\]"))
				  (:capture (:many :nbsp :word))
				  (:nbsp (:capture ":"))))
     '(1 font-lock-builtin-face) '(2 font-lock-type-face)
     '(3 font-lock-keyword-face)
     '(4 font-lock-variable-name-face)
     '(5 font-lock-builtin-face))

   (curly-keyword (:bol (:capture "module")
			(:optional ((:capture (:many :nbsp :word)) :spc (:capture ":")))
			(:capture "[^\n]*"))
     '(1 font-lock-builtin-face)
     '(2 font-lock-keyword-face nil t)
     '(3 font-lock-builtin-face nil t)
     '(4 font-lock-doc-face))

   (list "^\\(define\\|operator\\|function\\|let\\)\\s-+\\(\\sw+\\)"
         '(1 font-lock-builtin-face) '(2 font-lock-function-name-face)
         '(curly-find-argument (save-excursion (curly-find-arguments-end) (point)) nil
                               (1 font-lock-variable-name-face nil t)
			       (2 font-lock-function-name-face nil t)))


   (list "[{}]" 0 font-lock-function-name-face)
   (list "_" 0 font-lock-builtin-face)
   (cons "[()]" font-lock-builtin-face)
   (list "[0-9][0-9]*" 0 font-lock-constant-face)
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
	   (2 font-lock-function-name-face t t)))))

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

(defun curly-extend-after-change-region (beg end old-len)
  (save-match-data
    (save-excursion
      (cons
       (progn
	 (goto-char beg)
	 (while (progn (beginning-of-line) (looking-at "\\s-"))
	   (backward-char))
	 (point))
       (progn
	 (goto-char end)
	 (while (progn (end-of-line) (looking-at "\n\\s-"))
	   (forward-char))
	 (point))))))

(defun curly-interactive-session ()
  (interactive)
  (let ((path (reverse (split-string (buffer-file-name) "/")))
        exec)
    (while (not (null path))
      (setq exec (mapconcat 'identity (reverse (cons ".curly" path)) "/"))
      (if (file-exists-p exec) (setq path nil) (setq path (cdr path))))
    (let ((bname (concat "curly:" exec)))
      (setenv "EDITOR" "emacsclient")
      (switch-to-buffer (get-buffer-create (concat "*" bname "*")))
      (make-term bname exec nil "--interactive")
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
    (when (looking-back "^[ \t]*" nil) (skip-chars-forward " \t"))))

(defun curly-post-self-insert-function ()
  (when (looking-back "\\.\\.\\." nil)
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
  (set (make-local-variable 'font-lock-extend-after-change-region-function) 'curly-extend-after-change-region)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "#")
  (set (make-local-variable 'comment-style) 'plain)
  (add-hook (make-local-variable 'post-self-insert-hook) 'curly-post-self-insert-function)
  (local-set-key (kbd "C-c C-i") 'curly-interactive-session))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.c\\(url\\)?y\\'" . curly-mode))

(provide 'curly-mode)
