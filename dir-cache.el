;;
;; A filename cache implementation.
;;
;;
;; This Elisp plugin provides API for handling the following core objects:
;;
;;     - Linearized file path;
;;     - Abstract hierarchy tree;
;;     - Directory hierarchy tree.
;;
;; A linearized file path (also referred to as "chain") is a flat list of strings
;; acquired by splitting a filepath with `split-string', e.g. "/dev/shm" ->
;; '("dev" "shm").
;;
;; An abstract hierarchy tree is either a Lisp atom or a Lisp cons cell whose
;; CAR is a Lisp atom and whose CDR is a list of abstract hierarchy trees.
;; In the first case, the tree is a single node with no child nodes.
;; In the second case, CAR is treated as a parent node and CDR is treated as a
;; list of child nodes.
;;
;; A cons cell whose CDR is not a list, e.g. (atom . atom), is not a valid
;; hierarchy tree (nevertheless, it is allowed in a directory hierarchy tree,
;; representing a symlink).
;;
;; A cons cell whose CAR is not an atom, or a Lisp object which is not an atom
;; or a cons cell, is considered an invalid hierarchy tree.
;;
;; A directory hierarchy tree is a specific case of an abstract hierarchy tree.
;; It represents a directory structure.
;; All of its atoms are strings (= file/directory names).
;;
;; Therefore:
;;     - CAR of the whole tree holds the name of the the top-level directory.
;;     - A node that is a string represents a file.
;;     - A node that is a list represents a directory. The first element of this
;;   list should be a string. CAR of this list is the directory name and the
;;   list's CDR is the directory contents.
;;     - A node that is a cons cell represents a symlink. CAR of this cell is
;;   the link name and the cell's CDR is the symlink source. (Not implemented)
;;
;; Some examples:
;;
;; '(".emacs.d" "bookmarks")
;; '("var" ("empty") ("log" "debug" "dmesg" "messages"))
;;
;;
;; Note that the hierarchy trees representing a root directory "/" are special
;; among all other hierarchy trees. Since the root directory has no name, CAR
;; of the top node can be filled with any arbitrary string - this string will
;; be unused in the path construction anyway:
;;
;; ("ANY_STRING" ("etc" ("ssh" "ssh_config" "sshd_config")
;;               ("X11" ("xorg.conf.d"))))
;;
;; represents:
;;
;; etc
;; ├── ssh
;; │   ├── ssh_config
;; │   └── sshd_config
;; └── X11
;;     └── xorg.conf.d
;;
;;
;; In the current implementation, CAR of the top node holds a tree name, which is
;; an arbitrary string used for tree identification. The name of the default tree
;; is "default".
;;
;; Normally, there is more than one directory tree in an Emacs session because
;; the user might want to keep several independent filesets (e.g. one fileset
;; per a C++ project) and switch between them when necessary.
;;
;; These trees can be saved to disk and loaded from disk in the same way as Emacs
;; bookmarks do.
;;



;; Elisp API functions
(defun linearize-path (path)
  "Convert PATH to a linearized form."
  (let* ((path1 (file-name-noslash path))
		 (path2 (if (= (elt path1 0) ?\/) (substring path1 1) path1)))
	(split-string path2 "/")))



(defun subtree (tree path &optional destructive)
  "Retrieve a subtree of the directory tree TREE.
PATH is a linearized path (see above) to the subtree's top node.
If DESTRUCTIVE is set to true, then this function removes the specified subtree
if any, inserts a new subtree (with an empty tail) at the same location and
returns this new subtree.
Note that this subtree is intentionally made a reference to the parent tree,
therefore any modification of it affects the parent tree."
  (while (and path (or destructive tree))
	(setq child-tree (assoc (car path) (cdr tree)))

	(if (or (null destructive) child-tree)
		(setq tree child-tree path (cdr path))
	  (setq tree (nthcadr (1- (length path))
						  (car (append2 (cdr tree)(deflatten path))))
			path nil)))
  tree)



(defun dir-tree-print (tree)
  "Generate a list of paths to all nodes of the directory tree TREE."
  (let ((tree-name (if (listp tree) (car tree) tree)))
    (if (listp tree)
        (cons `(,tree-name nil)
              (apply 'append
                     (mapcar2 #'(lambda (node)
                                  (mapcar #'(lambda (child)
                                              (if (listp child)
                                                  (cons tree-name child)
                                                (list tree-name child)))
                                          (dir-tree-print node)))
                              (cdr tree))))
      (list tree-name))))



(defun dir-tree-gen (dirname &optional pattern)
  "Internal function.
Return DIRNAME's structure in a form of a tail of a hierarchy tree, concerning
only the files specified by PATTERN. The return result is supposed to be
attached to a node of an existing hierarchy tree with `setcdr'."
  (traverse-directory
   'file-name-nondirectory dirname pattern nil nil
   #'(lambda (file-results dir-results)
	   (append (mapcar
				#'(lambda (file-result) (cdr file-result))
				file-results)
			   (mapcar
				#'(lambda (dir-result)
					(cons (file-name-nondirectory (car dir-result))
						  (cdr dir-result)))
				dir-results)))))



(defun dir-tree-gen-ls (strings)
  "Internal function.
Create a tail of a directory hierarchy tree from the list of strings where each
string is an absolute filepath. The string list is typically acquired from 'ls'
output. The return result is supposed to be attached to a node of an existing
hierarchy tree with `setcdr'."
  (append (mapcar2 #'(lambda (root-elt)
					   (if (cdr (assoc root-elt strings))
						   (cons root-elt
								 (dir-tree-gen-ls
								  (mapcar2 #'(lambda (path)
											   (if (string= (car path)
															root-elt)
												   (cdr path)))
										   strings)))
						 (if (> (length root-elt) 0) root-elt)))
				   (delete-dups (mapcar 'car strings)))))



(defun dir-tree-get-completion-candidates (tree input)
  "Internal function.
Get completion candidates for INPUT from TREE, which is a directory tree.

The candidate list will include those paths to TREE nodes for which INPUT is
 a substring."
  (let* ((prefix (if (> (length input) 0)
					 (let ((i (1- (length input))))
					   (while (and (/= (elt input i) ?\/) (> i 0))
						 (setq i (1- i)))
					   (substring input 0 i))))
		 (prefix-chain (if (> (length prefix) 0) (split-string prefix "/")))
		 (suffix (if prefix-chain (substring input (1+ (length prefix))) input))
		 (tree (if prefix-chain (subtree tree prefix-chain) tree)))

	(mapcar2 #'(lambda (node)
				 (let ((node-name (if (listp node) (concat (car node) "/")
									node)))
				   (if (string-prefix-p suffix node-name completion-ignore-case)
					   (if prefix-chain (concat prefix "/" node-name)
						 node-name))))
			 (cdr tree))))



(defun dir-tree-get-completion-candidates-apropos (tree input)
  "Internal function.
Act like `dir-tree-get-completion-candidates' but use apropos completion
instead of prefix completion (that makes it much slower)."

  (defun match-fn (tree input-chain)
	;; Local function. Returns subdirectories of TREE that match INPUT-CHAIN,
	;; producing the result in a form of a list of paths.
	(apply 'append
		   (mapcar2 #'(lambda (node)
						(let* ((node-name (if (listp node) (car node) node))
							   (match (if (cdr input-chain)
										  (if completion-ignore-case
											  (string-equal
											   (downcase (car input-chain))
											   (downcase node-name))
											(string-equal (car input-chain)
														  node-name))
										(substring-p (car input-chain)
													 node-name
													 completion-ignore-case))))
						  (if (listp node)
							  (if (and match (null (cdr input-chain)))
								  (dir-tree-print node)
								(mapcar2 #'(lambda (chain)
											 (if chain (cons node-name chain)))
										 (match-fn node
												   (if match (cdr input-chain)
													 initial-chain))))
							(if match `((,node-name))))))
					(cdr tree))))

  (let ((initial-chain (split-string input "/")))
	(mapcar #'(lambda (chain) (mapconcat 'concat chain "/"))
			(match-fn tree initial-chain))))



(defun dir-tree-get (tree &optional prefix)
  "Read a file/directory name from a directory tree object.
If PREFIX is non-nil, prepend the return results with PREFIX."
  ;; Optimize the search by precalculating the maximal prefix.
  (let* ((prefix-chain (dir-tree-get-common-prefix tree))
         (tree (nthcadr (length prefix-chain) tree))
         (prefix (concat prefix (mapconcat 'concat prefix-chain "/"))))
    (concat prefix "/"
            (completing-read (format "File or directory at %s:\n" prefix)
                             (completion-table-dynamic
                              #'(lambda (input)
                                  (dir-tree-get-completion-candidates
                                   tree input)))))))



(defun dir-tree-get-apropos (tree &optional prefix)
  "Read a file/directory name from a directory tree object.
Act like `dir-tree-get' but use apropos completion instead of prefix
completion."
  ;; Optimize the search by precalculating the maximal prefix.
  (let* ((prefix-chain (dir-tree-get-common-prefix tree))
		 (tree (nthcadr (length prefix-chain) tree))
		 (prefix (concat prefix (mapconcat 'concat prefix-chain "/"))))

	(defun completion-fn (input pred action)
	  (let ((candidates (dir-tree-get-completion-candidates-apropos
						 tree input)))
		(cond ((null action) (ac-match-substring input candidates))
			  ((eq t action) candidates)
			  ((eq action 'metadata) (cons 'metadata nil))
			  ((eq 'lambda action) (member input candidates))
			  ((eq (car action) 'boundaries)
			   (cons (list 'boundaries 0) (length input)))
			  (t nil))))

	(concat prefix "/"
			(let ((completion-styles '(substring)))
			  (completing-read (format "File or directory at %s:\n" prefix)
							   'completion-fn)))))



(defun dir-tree-get-common-prefix (tree)
  "Internal function.
Descend the tree, calculating the common prefix for all files in the tree.
Return this prefix in a form of a linearized path."
  (let (pathvec)
  (while (and (listp tree) (eq (length (cdr tree)) 1))
    (setq pathvec (cons (car tree) pathvec))
	(setq tree (cadr tree)))

  (setq pathvec (cons (if (listp tree) (car tree) tree) pathvec))
  (cdr (reverse pathvec))))



(defun dir-tree-add (dir tree &optional wildcard)
  "Internal function.
Generate DIR's hierarchy tree and add it as a subtree to TREE, considering
only the files that match WILDCARD. WILDCARD can also be an abbreviation from
`grep-files-aliases'."
  (let ((pathvec (linearize-path dir)))
	(setcdr (subtree tree pathvec t)
			(dir-tree-gen dir (wildcard-to-regexp-extended wildcard)))))



;; Interactive commands for the end user.
;; Internally, these are just interactive wrappers for Elisp API; all they do
;; is applying Elisp API to a default directory tree, 'dir-cache-default'.
;;
;; For convenience, and in order to distinguish from Elisp API functions, all
;; the object/function names below start with 'dir-cache-' prefix.
(defconst dir-cache-default-name "default"
  "Name of the default cache object.")



(defvar dir-cache-default (list dir-cache-default-name)
  "The default filename cache, which is a root directory tree.")



(defconst dir-cache-default-dump "~/.emacs.d/dir-cache-default"
  "Name of the file that holds a dump of `dir-cache-default'.")



(defvar dir-cache-list-remote nil
  "A cache of remote filenames represented by an assoc list.
An element of this list is a root hierarchy tree whose CAR is the host name.")



(defconst dir-cache-list-remote-dump "~/.emacs.d/dir-cache-list-remote"
  "Name of the file that holds a dump of `dir-cache-list-remote'.")



(defun dir-cache-add (dir &optional wildcard)
  "Generate DIR's hierarchy tree and add it to the default cache
(`dir-cache-default'), considering only the files that match WILDCARD.
WILDCARD can also be an abbreviation from `grep-files-aliases'."
  (interactive "DEnter the directory to be cached: \n\
sEnter the regexp for the file names (all files by default): ")
  (dir-tree-add (expand-file-name (substring-no-properties dir))
				dir-cache-default
				wildcard))



(defun dir-cache-add-remote (host dir &optional wildcard)
  "Generate DIR's hierarchy tree and add it to the cache of the
remote host HOST (see `dir-cache-list-remote'), considering only
the files that match WILDCARD.
WILDCARD can also be an abbreviation from `grep-files-aliases'."
  (interactive (let* ((arg1
					   (completing-read "Enter the hostname: "
										dir-cache-list-remote))
					  (arg2
					   (dir-tree-get-apropos (assoc arg1 dir-cache-list-remote)
											 "/"))
					  (arg3
					   (read-string "Enter the regexp for the file names (all \
files by default): ")))
				 (list arg1 (substring-no-properties arg2) arg3)))

  (setq dir (file-name-noslash (expand-file-name dir)))

  (if (file-directory-p (format "/%s:%s" host dir))
	  (let* ((files (mapcar #'(lambda (filename)
								(split-string (substring filename
														 (1+ (length dir))) "/"))
							(split-string
							 (shell-command-to-string
							  (format
							   "ssh %s \"find %s %s | xargs -n1 -r ls --file-type -d\""
							   host
							   dir
							   (if (zerop (length wildcard)) ""
								 (mapconcat
								  #'(lambda (elt) (format "-name \\\"%s\\\"" elt))
								  (split-string wildcard " " t) " -o "))))
							 "\n" t)))
			 (hier (dir-tree-gen-ls files)))

		;; If dir-cache-list-remote is nil, create it.
		(unless dir-cache-list-remote
		  (setq dir-cache-list-remote (list (cons host nil))))

		;; Insert the new entry into the cache.
		(let ((pathvec (linearize-path dir)))
		  (setcdr (subtree (assoc host dir-cache-list-remote) pathvec t)
				  hier)))

	(error "Directory name error.")))



(defun dir-cache-get ()
  "Prompt for a file name using `dir-cache-default' for autocompletion, and
open the specified file.
When called with a prefix argument, additionally prompt for a filename for the
second time after the first filename is confirmed by RET, using this first
filename as a basedir.
This two-step prompt can be used to explicitly narrow down the candidates set."
  (interactive)
  (let ((completion-ignore-case t))
	(find-file (if current-prefix-arg
				   (let* ((dir (expand-file-name (dir-tree-get dir-cache-default
															   "/")))
						  (subtree (subtree dir-cache-default (linearize-path dir))))
					 (dir-tree-get-apropos subtree (file-name-as-directory dir)))
				   (dir-tree-get-apropos dir-cache-default "/")))))



(defun dir-cache-get-remote ()
  "Act like `dir-cache-get' but use the cache of remote directories instead
of the cache of local directories."
  (interactive)
  (let* ((completion-ignore-case t)
		 (host (completing-read "Enter the hostname: " dir-cache-list-remote))
		 (dir-cache (assoc host dir-cache-list-remote)))
	(find-file (format "/%s:%s" host
					   (if current-prefix-arg
						   (let ((dir (expand-file-name (dir-tree-get dir-cache
																	  "/"))))
							 (dir-tree-get-apropos
							  (subtree dir-cache (linearize-path dir)) dir))
						   (dir-tree-get-apropos dir-cache "/"))))))



(defun dir-cache-save ()
  "Save `dir-cache-default' to disk."
  (interactive)
  (save-var-to-disk dir-cache-default dir-cache-default-dump))



(defun dir-cache-load ()
  "Load `dir-cache-default' from disk."
  (interactive)
  (load-var-from-disk dir-cache-default dir-cache-default-dump))



(defun dir-cache-save-remote ()
  "Save `dir-cache-list-remote' to disk."
  (interactive)
  (save-var-to-disk dir-cache-list-remote dir-cache-list-remote-dump))



(defun dir-cache-load-remote ()
  "Load `dir-cache-list-remote' from disk."
  (interactive)
  (load-var-from-disk dir-cache-list-remote dir-cache-list-remote-dump))



(provide 'dir-cache)
