;;;; esrap-env.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package :esrap-liquid)

(cl-interpol:enable-interpol-syntax)

(defmacro in-esrap-env (symbol)
  (flet ((s (x) (symbolicate (string-upcase x))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmacro ,(s "define-rule") (symbol args &body body)
	 `(,',(if symbol
		  (s #?"define-$(symbol)-rule")
		  'defrule)
	      ,symbol ,args ,@body)))))

(defun install-common-rules (hash-table)
  (let ((common-rules '(any-string character string eof sof any-token)))
    (iter (for rule in common-rules)
	  (setf (gethash rule hash-table)
		(gethash rule *rules*)))))


(defun reintern-to-right-package (expression package)
  (if (and (consp expression)
	   (eql (car expression) 'quote)
	   (equal (length expression) 2)
	   (symbolp (cadr expression))
	   (not (keywordp (cadr expression))))
      `(quote ,(intern (string (cadr expression))
		       package))
      expression))


(defmacro define-esrap-env (symbol &key mainly-non-context)
  (flet ((s (x) (symbolicate (string-upcase x))))
  `(progn (eval-when (:compile-toplevel :load-toplevel :execute)
	    (defvar ,(s #?"$(symbol)-rules") (make-hash-table))
	    (install-common-rules ,(s #?"$(symbol)-rules"))
	    (defvar ,(s #?"$(symbol)-contexts") nil))
	  (defmacro ,(s #?"with-$(symbol)-rules") (&body body)
	    `(let ((esrap-liquid::*rules* ,',(s #?"$(symbol)-rules")))
	       ,@body))
	  (defmacro ,(s #?"with-$(symbol)-contexts") (&body body)
	    `(let ((esrap-liquid::contexts ,',(s #?"$(symbol)-contexts")))
	       ,@body))
	  ,(flet ((umm (name fun)
		       `(defmacro ,(s name) (symbol args &body body)		  
			  `(,',fun
			    ,symbol
			    ,args
			    ,',(s #?"$(symbol)-rules")
			    #+nil
			    (let (;;same as with-*-rules
				  (esrap-liquid::*rules* )
				  ;;same as with-*-contexts
				  ;;(esrap-liquid::contexts ,',(s #?"$(symbol)-contexts"))
				  ))
			    ,@body))))
		 (if mainly-non-context
		     `(progn ,(umm #?"define-$(symbol)-rule" 'def-nocontext-rule2)
			     ,(umm #?"define-c-$(symbol)-rule" 'defrule2))
		     `(progn ,(umm #?"define-$(symbol)-rule" 'defrule2)
			     ,(umm #?"define-nc-$(symbol)-rule" 'def-nocontext-rule2))))
	  (defmacro ,(s #?"register-$(symbol)-context")
	      (context-var &rest plausible-contexts)
	    (flet ((s (x) (symbolicate (string-upcase x))))
	      `(progn (defparameter ,context-var ,(make-keyword (car plausible-contexts)))
		      ,@(mapcar (lambda (context-name)
				  (let ((pred-name (s #?"$(context-name)-$(context-var)-p"))
					(rule-name (s #?"$(context-name)-$(context-var)")))
				    `(progn
				       (defun ,pred-name (x)
					 (declare (ignore x))
					 (equal ,context-var ,(make-keyword context-name)))
				       (,',(s #?"define-$(symbol)-rule") ,rule-name ()
					   ;; KLUDGE: probably, special reader syntax for defining rules
					   ;; will not work here anyway
					   (pred #',pred-name 't)
					   nil))))
				plausible-contexts)
		      (push ',context-var ,',(s #?"$(symbol)-contexts")))))
	  (defmacro ,(s #?"$(symbol)-parse")
	      (expression text &key (start nil start-p)
                          (end nil end-p)
                          (junk-allowed nil junk-allowed-p))
	    `(,',(s #?"with-$(symbol)-rules")
		 (,',(s #?"with-$(symbol)-contexts")
		     (parse ,(reintern-to-right-package expression ,*package*)
			    ,text
			    ,@(if start-p `(:start ,start))
			    ,@(if end-p `(:end ,end))
			    ,@(if junk-allowed-p
				  `(:junk-allowed ,junk-allowed))))))
	  (defmacro ,(s #?"$(symbol)-parse-stream") (expression stream &key  (junk-allowed nil junk-allowed-p))
	    `(,',(s #?"with-$(symbol)-rules")
		 (,',(s #?"with-$(symbol)-contexts")
		     (parse-stream ,(reintern-to-right-package expression ,*package*)
				   ,stream
				   ,@(if junk-allowed-p
					 `(:junk-allowed ,junk-allowed))))))
	  (defmacro ,(s #?"$(symbol)-parse-token-iter") (expression token-iter
							 &key  (junk-allowed nil junk-allowed-p))
	    `(,',(s #?"with-$(symbol)-rules")
		 (,',(s #?"with-$(symbol)-contexts")
		     (parse-token-iter ,(reintern-to-right-package expression ,*package*)
				       ,token-iter
				       ,@(if junk-allowed-p
					     `(:junk-allowed ,junk-allowed))))))
	  (defmacro ,(s #?"mk-$(symbol)-tokenizer") (expression token-iter
						     &key  (junk-allowed nil junk-allowed-p))
	    `(,',(s #?"with-$(symbol)-rules")
		 (,',(s #?"with-$(symbol)-contexts")
		     (mk-tokenizer ,(reintern-to-right-package expression ,*package*)
				   ,token-iter
				   ,@(if junk-allowed-p
					 `(:junk-allowed ,junk-allowed))))))
	  )))
	  

			

;; This is the example of macroexpansion
#+nil
(define-esrap-env yaclyaml)
