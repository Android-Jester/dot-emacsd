(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place. If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher `(funcall ,fetcher ,elt ,list) elt) ,list)))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.
ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
	(push docstring body)
	(setq docstring nil))
  (let (where-alist)
	(while (keywordp (car body)) (push `(cons ,(pop body) (ensure-list ,(pop body))) where-alist))
	`(progn
	   (defun ,symbol ,arglist ,docstring ,@body)
	   (dolist (targets (list ,@(nreverse where-alist)))
		 (dolist (target (cdr targets))
		   (advice-add target (car targets) #',symbol))))))
