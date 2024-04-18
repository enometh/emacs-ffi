;;; -*- lexical-binding: t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Apr 18 17:24:45 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;; ffi-kitchin-extra.el - Modified ffi define macros
;;;
;;; additions extracted from github.com/jkitchin/emacs-modules
;;; zeromq/zeromq.org
;;; [[id:A2B7F051-EA53-4882-A978-05FAD211BB81]]

(require 'cl-lib)
(require 'ffi)

(defmacro define-ffi-library (symbol name)
  "Create a pointer named to the c library."
  (let ((library (cl-gensym))
	(docstring (format "Returns a pointer to the %s library." name)))
    (set library nil)
    `(defun ,symbol ()
       ,docstring
       (or ,library
	   (setq ,library (ffi--dlopen ,name))))))

(defmacro define-ffi-function (name c-name return args library &optional docstring)
  "Create an Emacs function from a c-function.
NAME is a symbol for  the emacs function to create.
C-NAME is a string of the c-function to use.
RETURN is a type-keyword or (type-keyword docstring)
ARGS is a list of type-keyword or (type-keyword name &optional arg-docstring)
LIBRARY is a symbol usually defined by `define-ffi-library'
DOCSTRING is a string for the function to be created.

An overall docstring is created for the function from the arg and return docstrings.
"
  ;; Turn variable references into actual types; while keeping
  ;; keywords the same.
  (let* ((return-type (if (keywordp return)
			  return
			(car return)))
	 (return-docstring (format "Returns: %s (%s)"
				   (if (listp return)
				       (cl-second return)
				     "")
				   return-type))
	 (arg-types (vconcat (mapcar (lambda (arg)
				       (if (keywordp arg)
					   (symbol-value arg)
					 ;; assume list (type-keyword name &optional doc)
					 (symbol-value (car arg))))
				     args)))
	 (arg-names (mapcar (lambda (arg)
			      (if (keywordp arg)
				  (cl-gensym)
				;; assume list (type-keyword name &optional doc)
				(second arg)))
			    args))
	 (arg-docstrings (mapcar (lambda (arg)
				   (cond
				    ((keywordp arg)
				     "")
				    ((and (listp arg) (= 3 (length arg)))
				     (cl-third arg))
				    (t "")))
				 args))
	 ;; Combine all the arg docstrings into one string
	 (arg-docstring (mapconcat 'identity
				   (cl-mapcar (lambda (name type arg-doc)
					      (format "%s (%s) %s"
						      (upcase (symbol-name name))
						      type
						      arg-doc))
					    arg-names arg-types arg-docstrings)
				   "\n"))
	 (function (cl-gensym))
	 (cif (ffi--prep-cif (symbol-value return-type) arg-types)))
    (set function nil)
    `(defun ,name (,@arg-names)
       ,(concat docstring "\n\n" arg-docstring "\n\n" return-docstring)
       (unless ,function
	 (setq ,function (ffi--dlsym ,c-name (,library))))
       ;; FIXME do we even need a separate prep?
       (ffi--call ,cif ,function ,@arg-names))))

(provide 'ffi-jkitchin-extra)
