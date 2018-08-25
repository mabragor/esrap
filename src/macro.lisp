;;;; macro.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package #:esrap-liquid)

(cl-interpol:enable-interpol-syntax)

(defvar *rule-stack* nil)

#+nil
(defmacro descend-with-rule (o!-sym &rest args)
  (with-gensyms (g!-it g!-got)
    (once-only (o!-sym)
      `(multiple-value-bind (,g!-it ,g!-got) (gethash ,o!-sym *rules*)
	 (if ,g!-got
	     (let ((*rule-stack* (cons ,o!-sym *rule-stack*)))
	       (tracing-level
		 (funcall ,g!-it ,@args)))
	     (error "Undefined rule: ~s" ,o!-sym)
	     )))))

(defun apply-rule-fun (fun &optional (args nil))
  (tracing-level
    (apply fun args)))

(defparameter *traced* t)
(defparameter *debug-trace-rules* nil)
(defparameter *not-tracing* (make-hash-table))
(defun descend-with-rule (name &rest args)
  (labels ((descend ()
	     (multiple-value-bind (it got) (gethash name *rules*)
	       (if got
		   (let ((*rule-stack* (cons name *rule-stack*)))
		     (apply-rule-fun it args))
		   (error "Undefined rule: ~s" name))))
	   (dispatch ()
	     (if (gethash name *not-tracing*)
		 (let ((*traced* nil))
		   (descend))
		 (descend))))
    (if (and *debug-trace-rules*
	     *traced*)
	(progn
	  (format t "~&~a(~a"
		  (make-string *tracing-indent* :initial-element #\space)
		  name)
	  (multiple-value-prog1 (dispatch)
	    (format t ")")))
	(dispatch))))

(defmacro the-position-boundary (&body body)
  `(let ((the-position (+ the-position the-length))
	 (the-length 0))
     ,@body))

(defparameter *cap-stash* nil
  "Assoc list used to capture temporary variables")

(defmacro with-fresh-cap-stash (&body body)
  "Extra level of indirection in *CAP-STASH* is needed to be able to insert new values in it"
  `(let* ((up-cap-stash *cap-stash*)
	  (*cap-stash* (cons nil nil)))
     (declare (ignorable up-cap-stash))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %propagate-cap-stash-upwards-meat (up-var down-var)
      (iter (for (key . val) in (car down-var))
	    ;; (format t "Propagating ~a ~a ... " key val)
	    (let ((it (assoc key (car up-var))))
	      (if it
		  (progn ;; (format t "update old~%")
		    (setf (cdr it) val))
		  (progn ;; (format t "create new~%")
		    (push (cons key val) (car up-var)))))))
  (defun %propagate-cap-stash-upwards-meat-nil (up-var down-var)
    (progn (%propagate-cap-stash-upwards-meat up-var down-var) nil))
  
  (defun propagate-cap-stash-upwards (up-var down-var body)
    (if (not body)
	`(%propagate-cap-stash-upwards-meat-nil ,up-var ,down-var)	
	`(multiple-value-prog1 (progn ,@body)
	   (%propagate-cap-stash-upwards-meat ,up-var ,down-var)))))

(defmacro with-sub-cap-stash (&body body)
  `(with-fresh-cap-stash
     ,(propagate-cap-stash-upwards 'up-cap-stash '*cap-stash* body)))

(defun %wrap-with-esrap-macrolets-v (thing args)
  (cond ((characterp thing) (if args
				(error "Descent with character has extra argument, &
                                                                  but it shouldn't")
				`(descend-with-rule 'character ,thing)))
	((stringp thing) (if args
			     (error "Descent with string has extra argument, &
                                                               but it shouldn't")
			     `(descend-with-rule 'string ,thing)))
	((symbolp thing) `(descend-with-rule ',thing ,@args))
	(t (error "Don't know how to descend with this : ~a" thing))))

;;;cap
(defun %%cap (key val)
  (let ((it (assoc key (car *cap-stash*))))
    (if it
	(setf (cdr it)
	      val)
	(push (cons key val)
	      (car *cap-stash*)))))
(defun %wrap-with-esrap-macrolets-cap (key val)
  (let ((key (intern (string key) "KEYWORD")))
    `(%%cap ,key ,(maybe-wrap-in-descent val))))

;;;recap
(defun %%recap (key)
  (let ((it (assoc key (car *cap-stash*))))
    (if it
	(cdr it)
	(fail-parse-format "Key ~a is not captured (unbound)." key))))
(defun %wrap-with-esrap-macrolets-recap (key)
  (let ((key (intern (string key) "KEYWORD")))
    `(%%recap ,key)))

;;;recap?
(defun %wrap-with-esrap-macrolets-recap? (key)
  `(handler-case (recap ,key)
     (internal-esrap-error () nil)))

(defun wrap-with-esrap-macrolets (body)
  `(macrolet ((v (thing &rest args)
		(%wrap-with-esrap-macrolets-v thing args))
	      (cap (key val)
		(%wrap-with-esrap-macrolets-cap key val))
	      (recap (key)
		(%wrap-with-esrap-macrolets-recap key))
	      (recap? (key)
		(%wrap-with-esrap-macrolets-recap? key)))
     ,body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun maybe-wrap-in-descent (thing)
    (cond ((characterp thing) `(descend-with-rule 'character ,thing))
	  ((stringp thing) `(descend-with-rule 'string ,thing))
	  ((symbolp thing) `(descend-with-rule ',thing))
	  (t thing))))

(defun make-anonymous-rule-lambda (name args body)
  (multiple-value-bind
	(name defun)
      (make-rule-lambda
       name args body)
    (declare (ignorable name))
    ;;FIXME::fragile hack based on position of defun in defrule
    (progn
      (setf (first (third defun)) 'lambda)
      (setf (cdr (third defun))
	    (cddr (third defun))))
    defun))

(defun make-rule-lambda (name args body)
  (multiple-value-bind (reqs opts rest kwds allow-other-keys auxs kwds-p)
      (parse-ordinary-lambda-list args)
    (declare (ignore kwds))
    (if kwds-p (error "&KEY arguments are not supported"))
    (if allow-other-keys (error "&ALLOW-OTHER-KEYS is not supported"))
    (if auxs (error "&AUX variables are not supported, use LET"))
    (multiple-value-bind (body decls doc) (parse-body body :documentation t)
      (let ((defun-name (intern #?"GENERATED-ESRAP-$((string name))")))
	(values
	 defun-name
	 (wrap-with-esrap-macrolets
	  `(defun ,defun-name (,@args)
	     ,@(if doc `(,doc))
	     ,@decls
	     (with-fresh-cap-stash
	       (with-cached-result
		   (,name ,@reqs
			  ,@(if rest
				`(,rest)
				(iter (for (opt-name opt-default opt-supplied-p) in opts)
				      (collect opt-name)
				      (if opt-supplied-p
					  (collect opt-supplied-p)))))
		 ,@body)))))))))


(defun %set-rule (name lambda)
  (setf (gethash name *rules*)
	lambda))
(defun %set-sensitivity-t (name)
  (setf (gethash name *rule-context-sensitivity*) t))
(defun %set-sensitivity-nil (name)
  ;;FIXME::shouldn't this be wrapped the same way esrap-liquid::*rules* is wrapped?
  (setf (gethash name *rule-context-sensitivity*) nil))

;;;;API that depends on dynamic value of *rules*
(defmacro %defrule (name args &body body)
  (if-debug-fun "I'm starting to actually expand ~a!" name)
  ;; TODO: bug - C!-vars values are kept between different execution of a rule!
  (multiple-value-bind (fun-name def) (make-rule-lambda name args body)
    `(progn
       ,def
       (%set-rule ',name
		  ',fun-name))))
(defmacro defrule (name args &body body)
  `(progn (%defrule ,name ,args ,@body)
	  (%set-sensitivity-t ',name)))
(defmacro def-nocontext-rule (name args &body body)
  `(progn (%defrule ,name ,args ,@body)
	  (%set-sensitivity-nil ',name)))

;;;;;API that does not depend on dynamic value of rules, its explicit.
(defmacro %defrule2 (name args rules &body body)
  (if-debug-fun "I'm starting to actually expand ~a!" name)
  ;; TODO: bug - C!-vars values are kept between different execution of a rule!
  (multiple-value-bind (fun-name def) (make-rule-lambda name args body)
    `(progn
       ,def
       (let ((esrap-liquid::*rules* ,rules))
	 (%set-rule ',name
		    ',fun-name)))))
(defmacro defrule2 (name args rules &body body)
  `(progn (%defrule2 ,name ,args ,rules ,@body)
	  (%set-sensitivity-t ',name)))
(defmacro def-nocontext-rule2 (name args rules &body body)
  `(progn (%defrule2 ,name ,args ,rules ,@body)
	  (%set-sensitivity-nil ',name)))


(defmacro make-result (result &optional (length 0) beginning)
  (with-gensyms (g!-result)
    ;; We must preserve the semantics, that computation of results occurs before increment of length
    `(let ((,g!-result ,result))
       (incf the-length ,length)
       (if-debug "~asuccess: ~s ~a" ,(if beginning
					 #?"$(beginning) "
					 "")
		 ,g!-result the-length)
       ,g!-result)))


(defmacro || (&rest clauses)
  (with-gensyms (g!-result g!-the-length g!-ordered-choice ;;g!-parse-errors
			   )
    `(tracing-level
       (if-debug "||")
       (multiple-value-bind (,g!-result ,g!-the-length)
	   ;; All this tricky business with BLOCK just for automatic LENGTH tracking.
	   (block ,g!-ordered-choice
	     (progn ;;let (,g!-parse-errors)
	       ,@(mapcar (lambda (clause)
			   `(the-position-boundary
			      (print-iter-state)
			      (with-saved-iter-state (the-iter)
				(handler-case (return-from ,g!-ordered-choice
						(let ((res (with-sub-cap-stash
							     ,(maybe-wrap-in-descent clause))))
						  ;; (if-debug "|| pre-succeeding")
						  (values res the-length)))
				  (internal-esrap-error (e)
				    (declare (ignorable e))
				    (restore-iter-state)
				    ;;(push e ,g!-parse-errors)
				    )))))
			 clauses)
	       (if-debug "|| before failing P ~a L ~a" the-position the-length)
	       (fail-parse "Optional parse failed")))
	 (if-debug "|| aftermath ~a ~a" the-length ,g!-the-length)
	 (incf the-length ,g!-the-length)
	 ,g!-result))))

(defmacro most-full-parse (&rest clauses)
  (with-gensyms (g!-result g!-the-length g!-successful-parses g!-parse-errors g!-most-full-parse)
    `(tracing-level
       (if-debug "MOST-FULL-PARSE")
       (multiple-value-bind (,g!-result ,g!-the-length)
	   ;; All this tricky business with BLOCK just for automatic LENGTH tracking.
	   (block ,g!-most-full-parse
	     (let (,g!-parse-errors ,g!-successful-parses)
	       ,@(mapcar (lambda (clause)
			   `(the-position-boundary
			      (print-iter-state)
			      (with-saved-iter-state (the-iter)
				(with-fresh-cap-stash
				  (handler-case ,(maybe-wrap-in-descent clause)
				    (internal-esrap-error (e)
				      (restore-iter-state)
				      (push e ,g!-parse-errors))
				    (:no-error (res)
				      (restore-iter-state)
				      (push (list res the-length *cap-stash*) ,g!-successful-parses)))))))
			 clauses)
	       (if ,g!-successful-parses
		   (destructuring-bind (res length stash) (car (sort ,g!-successful-parses #'> :key #'cadr))
		     ,(propagate-cap-stash-upwards '*cap-stash* 'stash nil)
		     (fast-forward the-iter length)
		     (values res length))
		   (progn (if-debug "|| before failing P ~a L ~a" the-position the-length)
			  (fail-parse "MOST-FULL-PARSE failed.")))))
	 (if-debug "MOST-FULL-PARSE aftermath ~a ~a" the-length ,g!-the-length)
	 (incf the-length ,g!-the-length)
	 ,g!-result))))


(defmacro ! (expr)
  "Succeeds, whenever parsing of EXPR fails. Does not consume, returns NIL, for compatibility with TEXT"
  `(tracing-level
     (if-debug "! P ~a L ~a" the-position the-length)
     (the-position-boundary
       (with-saved-iter-state (the-iter)
	 (handler-case (let ((positive-mood nil))
			 (with-fresh-cap-stash ,(maybe-wrap-in-descent expr)))
	   (internal-esrap-error ()
	     (restore-iter-state)
	     nil)
	   (:no-error (result)
	     (declare (ignore result))
	     (fail-parse "Clause under non-consuming negation succeeded.")))))
     (if-debug "! before result P ~a L ~a" the-position the-length)
     (make-result nil 0)))

(defmacro !! (expr)
  "Succeeds, whenever parsing of EXPR fails. Consumes, assumes than EXPR parses just one character."
  `(tracing-level
     (if-debug "!!")
     (the-position-boundary
       (with-saved-iter-state (the-iter)
	 (handler-case (let ((positive-mood nil))
			 (with-fresh-cap-stash ,(maybe-wrap-in-descent expr)))
	   (internal-esrap-error ()
	     (restore-iter-state)
	     nil)
	   (:no-error (result)
	     (declare (ignore result))
	     (fail-parse "Clause under consuming negation succeeded.")))))
     (descend-with-rule 'any-token)))
              

(defmacro times (subexpr &key from upto exactly)
  (with-gensyms (g!-result g!-i g!-subresult g!-the-length)
    (flet ((frob (condition)
	     `(let (,g!-result)
		(tracing-level
		  (iter ,(if (or upto exactly)
			     `(for ,g!-i from 1 to ,(or upto exactly)))
			(if-debug "TIMES")
			;; (print-iter-state the-iter)
			(multiple-value-bind (,g!-subresult ,g!-the-length)
			    (with-saved-iter-state (the-iter)
			      ;; (format t "   Inside subexpression:~%")
			      (handler-case (the-position-boundary
					      (let ((subexpr (with-sub-cap-stash ,(maybe-wrap-in-descent subexpr))))
						;; (format t "    succeeding ~s ~a~%" subexpr the-length)
						;; (print-iter-state the-iter)
						(values subexpr the-length)))
				(internal-esrap-error ()
				  ;; (format t "    failing~%")
				  (restore-iter-state)
				  (finish))))
			  (if-first-time nil
					 (if (equal ,g!-the-length 0)
					     (terminate)))
			  (push ,g!-subresult ,g!-result)
			  (incf the-length ,g!-the-length))
			(finally (if ,condition
				     (return (make-result (nreverse ,g!-result)))
				     (fail-parse "Greedy repetition failed."))))))))
      (cond (exactly (if (or from upto)
			 (error "keywords :EXACTLY and :FROM/:UPTO are mutually exclusive.")
			 (frob `(equal (length ,g!-result) ,exactly))))
	    (from (if upto
		      (frob `(and (>= (length ,g!-result) ,from)
				  (<= (length ,g!-result) ,upto)))
		      (frob `(>= (length ,g!-result) ,from))))
	    (upto (frob `(<= (length ,g!-result) ,upto)))
	    (t (frob t))))))
  
(defmacro postimes (subexpr)
  `(times ,subexpr :from 1))

(defmacro pred (predicate subexpr)
  (with-gensyms (g!-it)
    `(tracing-level
       (if-debug "PREDICATE")
       (with-sub-cap-stash
	 (let ((,g!-it (let ((positive-mood nil))
			 ,(maybe-wrap-in-descent subexpr))))
	   (if (funcall ,predicate ,g!-it)
	       ,g!-it
	       (fail-parse "Predicate test failed")))))))

(defmacro progm (start meat end)
  "Prog Middle."
  `(progn ,(maybe-wrap-in-descent start) (prog1 ,(maybe-wrap-in-descent meat) ,(maybe-wrap-in-descent end))))

(defmacro progn-v (&rest forms)
  "PROGN with automatic descent wrapping."
  `(progn ,@(mapcar #'maybe-wrap-in-descent forms)))

(defmacro prog1-v (&rest forms)
  "PROG1 with automatic descent wrapping."
  `(prog1 ,@(mapcar #'maybe-wrap-in-descent forms)))

(defmacro list-v (&rest args)
  "LIST with automatic descent wrapping."
  `(list ,@(mapcar #'maybe-wrap-in-descent args)))


(defmacro ? (subexpr)
  (with-gensyms (g!-? g!-result g!-the-length)
    `(tracing-level
       (if-debug "? ~a ~a" the-position the-length)
       (multiple-value-bind (,g!-result ,g!-the-length)
	   (block ,g!-?
	     (the-position-boundary
	       (print-iter-state)
	       (with-saved-iter-state (the-iter)
		 (handler-case (let ((positive-mood nil))
				 (with-sub-cap-stash ,(maybe-wrap-in-descent subexpr)))
		   (internal-esrap-error ()
		     (restore-iter-state)
		     (values nil nil))
		   (:no-error (result) (return-from ,g!-? (values result the-length)))))))
	 (when ,g!-the-length
	   (incf the-length ,g!-the-length))
	 ,g!-result))))

(defmacro & (subexpr)
  `(tracing-level
     (if-debug "&")
     (make-result (the-position-boundary
		    (with-saved-iter-state (the-iter)
		      (let ((it (with-sub-cap-stash ,(maybe-wrap-in-descent subexpr))))
			(restore-iter-state)
			it))))))
			

(defmacro -> (subexpr)
  `(tracing-level
     (if-debug "->")
     ,(if (and (symbolp subexpr) (equal (string subexpr) "EOF"))
	  `(progn (descend-with-rule 'eof) nil)
	  `(progn (the-position-boundary
		    (with-saved-iter-state (the-iter)
		      (with-sub-cap-stash ,(maybe-wrap-in-descent subexpr))
		      (restore-iter-state)))
		  (make-result nil)))))

(defmacro <- (subexpr)
  `(tracing-level
     (if-debug "<-")
     ,(if (and (symbolp subexpr) (equal (string subexpr) "SOF"))
	  `(progn (descend-with-rule 'sof) nil)
	  `(with-sub-cap-stash
	     (the-position-boundary
	       (handler-case (progn (rel-rewind the-iter)
				    (decf the-position))
		 (buffer-error ()
		   (fail-parse "Can't rewind back even by 1 token")))
	       ,(maybe-wrap-in-descent subexpr)
	       (if (not (equal the-length 1))
		   (fail-parse "Parsing of subexpr took more than 1 token.")))
	     nil))))
	     

(defmacro cond-parse (&rest clauses)
  `(|| ,@(mapcar (lambda (clause)
                   `(progn ,@clause))
                 clauses)))

    
