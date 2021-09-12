(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-commonlisp-generator")
  (ql:quickload "alexandria"))
(in-package :cl-commonlisp-generator)



(progn
  (defparameter *path* "/home/martin/stage/cl-tcl-generator/")
  (defparameter *code-file* "t")
  (defparameter *source* (format nil "~a/" *path*))
  (defparameter *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))
  
  (let* (
	 (code
	   `(toplevel
	     "(in-package :cl-tcl-generator)"
	     (defparameter
		 _code_git_version
	       (string ,(let ((str (with-output-to-string (s)
				     (sb-ext:run-program "/usr/bin/git" (list "rev-parse" "HEAD") :output s))))
			  (subseq str 0 (1- (length str)))))
	       
	       )
	     (defparameter _code_generation_time
		 (string ,(multiple-value-bind
				(second minute hour date month year day-of-week dst-p tz)
			      (get-decoded-time)
			    (declare (ignorable dst-p))
			    (format nil "~2,'0d:~2,'0d:~2,'0d of ~a, ~d-~2,'0d-~2,'0d (GMT~@d)"
				    hour
				    minute
				    second
				    (nth day-of-week *day-names*)
				    year
				    month
				    date
				    (- tz)))))
	     (defparameter
		 _code_repository (string ,(format nil "https://github.com/plops/cl-verilog-generator/tree/master/gen-v.lisp")))
	     
	     (setf (readtable-case *readtable*) :invert)
	     (toplevel
	      (defparameter *file-hashes* (make-hash-table))
	      (defun write-source (name code &key
					       (dir (user-homedir-pathname))
					       ignore-hash
					       (format t))
		(let* ((fn (merge-pathnames (format nil (string "~a") name)
					    dir))
		       (code-str (emit-t :code code))
		       (fn-hash (sxhash fn))
		       (code-hash (sxhash code-str)))
		  (multiple-value-bind (old-code-hash exists) (gethash fn-hash *file-hashes*)
		    (when (or (not exists) ignore-hash (/= code-hash old-code-hash)
			      (not (probe-file fn)))
		      ;; store the sxhash of the verilog source in the hash table
		      ;; *file-hashes* with the key formed by the sxhash of the full
		      ;; pathname
		      (setf (gethash fn-hash *file-hashes*) code-hash)
		      (with-open-file (s fn
					 :direction :output
					 :if-exists :supersede
					 :if-does-not-exist :create)
			(write-sequence code-str s))
		      (when format
			
			("sb-ext:run-program" (string "/home/martin/stage/cl-tcl-generator/run_emacs_formatter.sh")
					      (list (namestring fn)))
			("sb-ext:run-program" (string "/usr/bin/sed")
					      (list (string "-i")
						    (string "/^[[:space:]]*$/d")
						    (namestring fn)
						    )))))))
	      )

	     (defun emit-t (&key
			      code
			      (level 0)
			      suffix)
	       (labels ((emit (code &key (dl 0)  suffix
				      )
			(emit-t :code code
				:level (+ dl level)
				:suffix suffix))
			(emits (code &key (dl 0) suffix
				       )
			  (if (listp code)
			      (mapcar #'(lambda (x) (emit x
							 :dl dl
							 :suffix suffix
							 ))
				      code)
			      (emit code
				       :dl dl
					:suffix suffix
				       ))))
		 (if code
		     (macrolet ((out (cmd &rest rest)
						      "`(format s ,cmd ,@rest)")
						    (outsemiln (cmd &rest rest)
						      "`(format s
							      (concatenate 'string
									   ,cmd
									   (format nil
										   \";~@[ // ~a~]~%\"
										   suffix))
							      ,@rest)"
						      )
						    (outln (cmd &rest rest)
						      "`(format s
							      (concatenate 'string
									   ,cmd
									   (format nil
										   \" ~@[ // ~a~]~%\"
										   suffix))
							      ,@rest)"
						      ))
		      (if (listp code)
			  ,(flet ((row (body)
				    `(destructuring-bind (name &rest args) code
				       (with-output-to-string (s)
					 ,body))))
			     `(case (car code)
				(comment
				 ,(row
				   `(setf suffix (first args))))
				(comma
				 ,(row `(out (string "~{~a~^, ~}") (emits args))))
				(paren
				 ,(row `(out (string "(~{~a~^, ~})") (emits args))))
				(concat
				 ,(row `(out (string "{~{~a~^, ~}}") (emits args))))
				(space
				 ,(row `(out (string "~{~a~^, ~}") (emits args))))
				
				,@(loop for op in `(or (and "&") 
						       + -
						       (logior "||") (logand "&&")) ;; operators with arbitrary number of arguments
					collect
					(if (listp op)
					    (destructuring-bind (lisp-name verilog-name) op
					      `(,lisp-name
						,(row `(out (string ,(format nil "~~{(~~a)~~^ ~a ~~}" verilog-name))
							    (emits args)))))
					    `(,op
					      ,(row `(out (string ,(format nil "~~{(~~a)~~^ ~a ~~}" op))
							  (emits args))))))
				,@(loop for op in `(< <= ==) ;; operators with two arguments
					collect
					`(,op
					  ,(row `(out (string ,(format nil "(~~a) ~a (~~a)" op))
						      (emit (first args))
						      (emit (second args))))))
				
				
				(setf
				 ,(row
				   `(loop for (a b) on args by #'cddr
					  collect
					  (outsemiln (string "set ~a ~a")
						     a b))
				   ))
				#+nil 
				(incf
				 ,(row
				   `(destructuring-bind (target &optional (increment 1)) args
				      (out (string "~a")
					   (emit "`(setf ,target (+ ,target ,increment))")))
				   ))
				#+nil (not
				 ,(row `(out (string "(! (~a))")
					     (emit (elt args 0)))))
				#+nil (aref ,(row
					`(destructuring-bind (name &rest indices) args
					   (out (string "~a[~{~a~^,~}]")
						(emit name)
						(emits indices)))))
				#+nil				(slice
				 ,(row
				   `(out (string "~a:~a")
					 (emit (first args))
					 (emit (second args))))
				 )
				#+nil (cond
				  ,(row `(loop for clause in args
					       and ci from 0
					       collect
					       (destructuring-bind (condition &rest body) clause
						 (if (eq ci 0)
						     (outln (string "if (~a) begin")
							    (emit condition))
						     (if (eq condition t)
							 (outln (string "else begin")
								)
							 (outln (string "else if (~a) begin")
								(emit condition))))
						 (loop for b in body
						       do
							  (outln (string "~a") (emit b)))
						 (outln (string "end")))
					       )))
				#+nil (if
				 ,(row `(destructuring-bind (condition
							     true-statement
							     &optional
							       false-statement)
					    args
					  (outln (string "if (~a) begin") (emit condition))
					  (outln (string "~a") (emit true-statement))
					  (outln (string "end"))
					  (when false-statement
					    (outln (string "else begin"))
					    (outln (string "~a") (emit false-statement))
					    (outln (string "end"))))))
				#+nil (?
				 ,(row `(destructuring-bind (condition true-expression false-expression)
					    args
					  (out (string "(~a) ? (~a) : (~a)")
					       (emit condition)
					       (emit true-expression)
					       (emit false-expression)))))
				#+nil (case
				    ,(row `(destructuring-bind (keyform
								&rest clauses)
					       args
					     
					     (outln (string "case (~a)") (emit keyform))
					     (loop for clause in clauses
						   do
						      (destructuring-bind (key &rest forms) clause
							(if (eq key t)
							    (outln (string "default: begin"))
							    (outln (string "~a: begin")  (emit key)))
							(loop for form in forms
							      do
							      (outsemiln (string "~a") (emit form)))
							(outln (string "end"))))
					     (outln (string "endcase"))
					  ))
				  )
				#+nil (make-instance
				 ,(row `(outsemiln (string "~a ~a")
						   (emit (first args))
						   (emit (second args)))))
				(do0
				 ,(row `(progn
					  
					  (loop for arg in args
						do
						(outln (string "~a") (emit arg)))
					  )))
				#+nil (progn
				  ,(row `(progn
					  (outln (string "begin"))
					  (loop for arg in args
						do
						(outsemiln (string "~a") (emit arg)))
					  (outln (string "end")))))
				(t ,(row `(if (listp name)
					      (string "lambda call not supported")
					      (let* ((positional (loop for i below (length args)
								       until (keywordp (elt args i))
								       collect
								       (elt args i)))
						     (plist (subseq args
								    (length positional)))
						     (props (loop for e in plist
								  by #'cddr
								  collect
								  e)))
						(out (string "~a~a") name
							(emit "`(paren ,@(append
									 positional
									 (loop for e in props collect
									       `(,(format nil \".~a\" e) ,(getf plist e)))))")))
					      #+nil (out (string "~a~a")
							 (emit name)
							 (emit "`(paren ,@args)"))))
				 )))
			  (cond
			    ((keywordp code)
			     (format nil (string "kw_~a") code))
			    ((symbolp code)
			     (format nil (string "~a") code))
			    ((stringp code)
			     (format nil (string "~a") code))
			    ((numberp code)
			     (cond
			       ((integerp code)
				(format nil (string "~a") code))
			       (t
				(string "float not supported")))))))
		     (string ""))))
	      
	     
	     
	     )))
    (cl-commonlisp-generator:write-source
     (format nil "~a/~a" *source* *code-file*)
     code)))
