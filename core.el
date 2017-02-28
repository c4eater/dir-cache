;;
;; A set of generic Elisp functions.
;;


(require 'cl)
(require 'grep)



(defmacro delete2 (elt list)
  "Delete the first found element of LIST whose car equals ELT, then return the
modified list.
The comparison is done with `equal'.
This operation is destructive."
  `(let* ((tail (member (assoc ,elt ,list) ,list)))
	 (if (cdr tail)
		 (let ((car2 (cadr tail))
			   (cdr2 (cddr tail)))
		   (setcar tail car2)
		   (setcdr tail cdr2))
	   (if tail (setf (nthcdr (1- (length ,list)) ,list) nil)))
	 ,list))



(defmacro deflatten (list)
  "Transform a flat list of length N into a tree of depth N."
  `(if (cdr ,list) (cons (car ,list) (list (deflatten (cdr ,list)))) ,list))



(defmacro append2 (result &rest args)
  "Act like `append' but modify RESULT by appending ARGS to it.
Return the modified RESULT."
  (if (eval result) (list 'setcdr (list 'last result) (cons 'list args))
	(list 'setf result (cons 'list args))))



(defun nthcadr (n arg)
  "Get cadr of cadr of cadr... of cadr of ARG, with nesting depth N.
If N is zero or less, just return ARG."
  (while (> n 0) (setq n (1- n) arg (cadr arg))) arg)



(defun string-suffix-p (str1 str2 &optional ignore-case)
  "Return non-nil if STR1 is a suffix of STR2.
If IGNORE-CASE is non-nil, the comparison is case-insensitive."
  (eq t (compare-strings str1 nil nil
						 str2 (- (length str2) (length str1)) (length str2)
						 ignore-case)))



(defun substring-p (str1 str2 &optional ignore-case)
  "Return t if STR1 is a substring of STR2, nil otherwise.
If IGNORE-CASE is non-nil, the match is case-insensitive."
  (let ((case-fold-search ignore-case)) (string-match str1 str2)))



(defun line-empty-p (&optional n)
  "Return t if the current line is empty or contains only whitespace characters.
With optional argument N, scan forward N - 1 lines first (scan backward if the
argument is negative)."
  (save-excursion
	(forward-line (if (null n) 0 n)) ;; Moves to the beginning of a line
	(null (search-forward-regexp "[^ \t]" (line-end-position) t 1))))



(defun read-lines-from-file (filepath)
  "Return the list of lines of the file specified by FILEPATH."
  (with-temp-buffer
	(insert-file-contents filepath)
	(split-string (buffer-string) "\n" t)))



(defmacro save-var-to-disk (var file)
  "Save VAR's value to FILE."
  `(with-temp-buffer
	 (let (print-level print-length)
	   (print ,var (current-buffer))
	   (write-region nil nil ,file))))



(defmacro load-var-from-disk (var file)
  "Load VAR's value from FILE."
  `(if (file-exists-p ,file)
	   (with-temp-buffer
		 (insert-file-contents-literally ,file)
		 (setq ,var (read (current-buffer))))))



(defun with-region-lines (region body)
  "Narrow to REGION which is a pair (BEG . END), then execute BODY at each line.
BODY is a function that should assume that the cursor is placed at the
beginning of the line each time."
  (save-restriction (narrow-to-region (car region) (cdr region))
					(goto-char 1)
					(funcall body) ;; First call
					(while (= 0 (forward-line 1)) (funcall body))))



(defun mapcar2 (mapcar2-ftor list)
  "Act like `mapcar' but concatenate only non-nil results."
  (let ((mapcar2-result ()))
	(dolist (mapcar2-arg list mapcar2-result)
	  (let ((returned-result (funcall mapcar2-ftor mapcar2-arg)))
		(if returned-result (setq mapcar2-result (cons returned-result
													   mapcar2-result)))))))



(defun lazy-mapcar (lazy-mapcar-ftor list)
  "Act like `mapcar' but terminate upon a non-nil result.
Return this result."
  (if list
	  (let ((tail list) (result nil))
		;; Drop the functional paradigm in favor of efficiency
		(while tail
		  (setq result (funcall lazy-mapcar-ftor (car tail)))
		  (setq tail (if result nil (cdr tail))))
		result)))



(defun file-name-noslash (path)
  "Strip the trailing slash from PATH."
  (let ((end-pos (- (length path) 1)))
  (if (char-equal (aref path end-pos) ?/) (substring path 0 end-pos) path)))



(defun wildcard-to-regexp-extended (wildcard)
  "Act like `wildcard-to-regexp' but also accept:

- A list of wildcards separated by space.
- A regexp abbreviation from `grep-files-aliases'."
  (if wildcard
	  (mapconcat 'wildcard-to-regexp
				 (split-string (or (cdr (assoc wildcard grep-files-aliases))
								   wildcard)
							   nil t)
				 "\\|")))



(defvar grep-find-ignored-files-backup grep-find-ignored-files
  "A cached value of `grep-find-ignored-files' used by
`grep-find-ignored-files-regex'.")



(defvar grep-find-ignored-files-regex-backup nil
  "A cached return result of `grep-find-ignored-files-regex'.")



(defun grep-find-ignored-files-regex ()
  "Convert `grep-find-ignored-files' into a single regex and return this regex.
Use a cached value wherever possible."
  (if (and grep-find-ignored-files-regex-backup
		   (equal grep-find-ignored-files-backup grep-find-ignored-files))
	  grep-find-ignored-files-regex-backup
	(setq grep-find-ignored-files-backup grep-find-ignored-files
		  grep-find-ignored-files-regex-backup
		  (mapconcat
		   #'(lambda (regexp)
			   (concat "\\(" (wildcard-to-regexp regexp) "\\)"))
		   grep-find-ignored-files
		   "\\|"))))



(defvar grep-find-ignored-directories-backup grep-find-ignored-directories
  "A cached value of `grep-find-ignored-directories' used by
`grep-find-ignored-directories-regex'.")



(defvar grep-find-ignored-directories-regex-backup nil
  "A cached return result of `grep-find-ignored-directories-regex'.")



(defun grep-find-ignored-directories-regex ()
  "Convert `grep-find-ignored-directories' into a single regex and return this
regex. Use a cached value wherever possible."
  (if (and grep-find-ignored-directories-regex-backup
		   (equal grep-find-ignored-directories-backup
				  grep-find-ignored-directories))
	  grep-find-ignored-directories-regex-backup
	(setq grep-find-ignored-directories-backup grep-find-ignored-directories
		  grep-find-ignored-directories-regex-backup
		  (mapconcat
		   #'(lambda (regexp)
			   (concat "\\(" (wildcard-to-regexp regexp) "\\)"))
		   grep-find-ignored-directories
		   "\\|"))))



(defun directory-files-and-attributes-nodots (directory &optional full match
														nosort id-format)
  "Act like `directory-files-and-attributes' but filter out \".\" and \"..\"."
  (mapcar2 #'(lambda (filespec)
			   (let* ((filepath (car filespec))
					  (filename (file-name-nondirectory filepath)))
				 (if (not (member filename '("." ".."))) filespec)))
		   (directory-files-and-attributes directory full match nosort
										   id-format)))



(defun get-directories-or-files (dir-flag dir-name &optional pattern
										  allow-garbage-files)
  "Internal function.
Process DIR-NAME and return either its files or its subdirectories.
If DIR-FLAG is non-nil, return a list of subdirectories.
Otherwise, return a list of normal files.
If PATTERN is non-nil, output only files/directories matching PATTERN.
If ALLOW-GARBAGE-FILES is non-nil, files/directories matching
`grep-find-ignored-files' and `grep-find-ignored-directories' are also
listed (normally they are filtered out)."

  (let ((garbage-files (directory-files dir-name nil
										(if dir-flag
											(grep-find-ignored-directories-regex)
										  (grep-find-ignored-files-regex))
										t)))

	(mapcar2 #'(lambda (filespec)
				 (if (eq (cadr filespec) dir-flag)
					 (let ((full-name (car filespec)))
					   (unless (and (null allow-garbage-files)
									(let ((name (file-name-nondirectory
												 full-name)))
									  (member name garbage-files)))
						 full-name))))
			 (directory-files-and-attributes-nodots dir-name t pattern t))))



(defun traverse-directory (ftor basedir &optional pattern lazy-match
								allow-garbage-files aggregator)
  "Process the directory BASEDIR recursively, applying FTOR to the files in
BASEDIR and below.

If PATTERN is non-nil, apply FTOR only to the files that match PATTERN.
If LAZY-MATCH is non-nil, terminate file processing (in the current directory)
when FTOR returns non-nil.
If ALLOW-GARBAGE-FILES is non-nil, files matching `grep-find-ignored-files' and
`grep-find-ignored-directories' are also processed (normally they are ignored).

AGGREGATOR is a function describing how the return values of
single FTOR calls are joined together to form the global return result.

This parameter can take the following values:

nil or 'simple-list - The results form a flat list.

'simple-tree - The results form a tree, according to the directory's hierarchy.

#'(lambda ...) - Explicit aggregator function. It should accept
two lists as arguments: the first list is a list of pairs like (FILENAME .
FTOR-RETURN-VALUE), and the second list is a list of pairs like (DIRNAME .
TRAVERSE-RETURN-VALUE), where FTOR-RETURN-VALUE is a result of (FTOR FILENAME)
and TRAVERSE-RETURN-VALUE is a result of (TRAVERSE-DIRECTORY FTOR DIRNAME ...).

Cryptic code FTW!"
  (let ((directory-list (get-directories-or-files t basedir nil
												  allow-garbage-files))
		(file-list (get-directories-or-files nil basedir pattern
											 allow-garbage-files))
		(aggregator-fn
		 (case aggregator
		   ((nil simple-list)
			;; An aggregator that builds a plain list.
			#'(lambda (file-results dir-results)
				(append (mapcar #'(lambda (result-pair) (cdr result-pair))
								file-results)
						(apply 'append
							   (mapcar #'(lambda (result-pair) (cdr result-pair))
									   dir-results)))))
		   (simple-tree
			;; An aggregator that builds a tree.
			#'(lambda (file-results dir-results)
				(append (mapcar #'(lambda (result-pair) (cdr result-pair))
								file-results)
						(mapcar #'(lambda (result-pair) (cdr result-pair))
								dir-results))))
		   (t
			;; A custom aggregator.
			aggregator)))) ;; Initialization in let-form ends here

	(funcall aggregator-fn
			 ;; First argument for the aggregator: list of pairs
			 ;; (FILENAME FTOR-RESULT).
			 (if file-list
				 (funcall (if lazy-match
							  #'(lambda (fn lst)
								  (let ((lazy-mapcar-result
										 (lazy-mapcar fn lst)))
									(if lazy-mapcar-result
										(list lazy-mapcar-result))))
							'mapcar2)
						  #'(lambda (file)
							  (cons file (funcall ftor file)))
						  file-list))
			 ;; Second argument for the aggregator: list of pairs
			 ;; (DIRNAME TRAVERSE-RESULT).
			 (if directory-list
				 (mapcar2 #'(lambda (dir)
							  (let ((traverse-result
									 (traverse-directory ftor dir pattern
														 lazy-match
														 allow-garbage-files
														 aggregator)))
								(if traverse-result (cons dir traverse-result))))
						  directory-list)))))



(defun fast-replace-regexp (regexp to-string &optional body)
  "Perform a regexp replacement like `replace-regexp' does, but operate faster
and without touching the mark ring. For each successful replacement,
execute BODY. If the replacement results in a blank line, remove this line.
Return the last value that has been substituted."
  (let ((replaced-value nil)
		(case-fold-search nil))
	(save-match-data
	  (goto-char 1)
	  (while (re-search-forward regexp nil t)
		(if (null replaced-value)
			(setq replaced-value (or (match-string 1)
									 (match-string 0))))
		(replace-match to-string t nil)
		(beginning-of-line)
		(if body (save-excursion (funcall body)))
		(unless (search-forward-regexp "\[^ \t\]" (line-end-position) t)
		  (delete-region (- (line-beginning-position) 1)
						 (line-end-position))))
	  replaced-value)))



(provide 'my/core)
