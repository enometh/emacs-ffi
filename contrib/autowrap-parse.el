;; -*- lexical-binding:t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Apr 21 15:00:06 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;;; cl-autowrap/autowrap/parse.lisp
;;
;; Copyright (c) 2013, Ryan Pavlik
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
(defun make-keyword (string)
  (cl-assert (not (eq (aref string 0) ?\:)))
  (intern (concat ":" string)))

(defvar *foreign-type-symbol-function* 'default-foreign-type-symbol)
(defvar *foreign-c-to-lisp-function* 'default-c-to-lisp)
(defvar *foreign-record-list* nil)
(defvar *foreign-alias-list* nil)
(defvar *foreign-function-list* nil)
(defvar *foreign-extern-list* nil)
(defvar *foreign-constant-list* nil)
(defvar *foreign-raw-constant-list* nil)
(defvar *foreign-constant-excludes* nil)
(defvar *foreign-other-exports-list* nil)
(defvar *foreign-symbol-exceptions* nil)
(defvar *foreign-symbol-regex* nil)

(defvar $autowrap-symbols '(*foreign-record-list*
			    *foreign-function-list* *foreign-extern-list*
			    *foreign-constant-list* *foreign-other-exports-list*
			    *foreign-alias-list*
			    *foreign-raw-constant-list*))

(defun init-symbols ()
  (mapcar (lambda (x) (set x nil)) $autowrap-symbols))

 ;; Collecting symbols

(cl-defmacro collecting-symbols (&body body)
  `(let (*foreign-record-list*
	 *foreign-function-list* *foreign-extern-list*
         *foreign-constant-list* *foreign-other-exports-list*
	 *foreign-alias-list*
         *foreign-raw-constant-list*)
     ,@body))



 ;; Types and symbols

(defun scan-to-strings (regexp string)
  (let* ((case-fold-search nil)
	 (ret
	  (when (string-match regexp string)
	    (cl-loop for i from 0
		     for beg = (match-beginning i)
		     for end = (match-end i)
		     until (not (and beg end))
		     collect (substring string beg end)))))
    (cl-values (car ret) (cdr ret))))

(when nil
(cl-equalp (scan-to-strings "[^b]*b" "aaabd") '("aaab" nil))
(cl-equalp (scan-to-strings "\\([^b]\\)*b" "aaabd") '("aaab" ("a")))
(cl-equalp (scan-to-strings "\\(\\([^b]\\)*\\)b" "aaabd") '("aaab" ("aaa" "a")))
)

(defun apply-regexps (string regex-list)
  (let* ((case-fold-search nil))
    (cl-loop for r in regex-list do
	     (cond ((functionp (cdr r))
		    (cl-multiple-value-bind (match matches)
			(scan-to-strings (car r) string)
		      (when match
			(setq string
			      (funcall (cdr r) string matches (car r))))))
		   ((stringp (cdr r))
		    (setq string
			  (replace-regexp-in-string (car r) (cdr r) string))))))
  string)

(defun default-c-to-lisp (string)
  (let* ((case-fold-search nil)
	 (string (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)"
					   "\\1_\\2" string nil nil nil)))
    (let ((string (replace-regexp-in-string
		   "\\([a-z]+\\)\\([A-Z]\\)"
		   "\\1_\\2" string nil nil nil )))
      (if (string-match "^\\(:_\\|_\\)" string)
          (downcase string)
          (downcase (cl-nsubstitute ?\- ?\_ string))))))


(defun foreign-symbol-exception-p (string)
  (and *foreign-symbol-exceptions*
       (cl-nth-value 1 (gethash string *foreign-symbol-exceptions*))))

(cl-defun default-foreign-type-symbol (string type &optional package)
  ;; package is an obarray should be nil
  (let ((string (or (and *foreign-symbol-exceptions*
                         (gethash string *foreign-symbol-exceptions*))
                    (and (eq type :cparam) string)
                    (funcall *foreign-c-to-lisp-function*
                             (if *foreign-symbol-regex*
                                 (apply-regexps string *foreign-symbol-regex*)
                               string)))))
    (if (eq ?\: (aref string 0))
	(make-keyword (cl-subseq string 1))
      (cond
       ((eq type :cconst)
        (intern (format  "+%s+" string) package))
       ((eq type :cenumfield)
	(make-keyword string))
       ((eq type :cfield)
	(make-keyword string))
       (t (intern string package))))))

(cl-defun foreign-type-symbol (string type &optional package)
  (if (string= "" string)
      (cl-case type
        (:cparam (gensym "P"))
        (:cfield (gensym "FIELD-"))
        (otherwise (gensym "ANON-TYPE-")))
      (funcall *foreign-type-symbol-function* string type package)))

(defvar *package* nil)

(defun make-record-ref (form)
  (let ((tag (alist-get 'tag form))
	(name (alist-get 'name form))
	(id (alist-get 'id form)))
    (let (symbol-type)
      (cond
        ((string= tag ":struct")
         (setq tag :struct)
         (setq symbol-type :cstruct))
        ((string= tag ":union")
         (setq tag :union)
         (setq symbol-type :cunion))
        ((string= tag ":enum")
         (setq tag :enum)
         (setq symbol-type :cenum))
        (t (error "Unknown tag for MAKE-RECORD-REF: %S" tag)))
      `((,tag (,(if (and (string= name "") (> id 0))
                    nil
                    (foreign-type-symbol name symbol-type *package*))
               ,@(when (and (string= name "") (> id 0))
                   `(:id ,id))))))))

(defun record-form-p (form)
  "Return whether FORM describes a struct or union"
  (let ((tag (alist-get 'tag form)))
   (or (string= "struct" tag)
       (string= "union" tag)
       (string= ":struct" tag)
       (string= ":union" tag))))

(defun pointer*-to-record-form-p (form)
  "If `FORM` describes a type which is a record, or one or more levels
of pointer-to-record"
  (let ((tag (alist-get 'tag form)))
    (if (string= ":pointer" tag)
        (pointer*-to-record-form-p (alist-get 'type form))
        (record-form-p form))))

(defun pointer-alias-form-p (form)
  "If `FORM` is an alias to a pointer."
  (let ((tag (alist-get 'tag form)))
    (cond
      ((string= tag "typedef") (pointer-alias-form-p (alist-get 'type form)))
      ((string= tag ":pointer") t)
      (t nil))))

(defun included-p (thing includes)
  (when thing
    (cl-loop for scanner in includes do
      (when (string-match scanner thing)
        (cl-return t)))))

(defun symbol-package (sym)
  (warn "SYMBOL PACKAGE %S" sym)
  nil)

(defun maybe-add-constant (name value)
  (push (cons name value) *foreign-raw-constant-list*)
  (unless (included-p name *foreign-constant-excludes*)
    (let ((sym (foreign-type-symbol name :cconst *package*)))
      (cl-pushnew sym *foreign-constant-list*)
      `(defparameter ,sym ,value))))


 ;; Parsing

(cl-defgeneric parse-type (form tag)
  (:documentation "Parse FORM describing a type, tagged by TAG.
Return the appropriate CFFI name."))

(cl-defmethod parse-type (form _tag)
  (list (foreign-type-symbol (alist-get 'tag form) :ctype *package*)))

(defun substr* (str start &optional end)
  ;; no displaced arrays
  (cl-subseq str start end))

(cl-defmethod parse-type :around (form tag)
  (cl-etypecase tag
    (symbol (cl-call-next-method))
    (string
     (parse-type form (if (eq ?\: (aref tag 0))
                          (make-keyword (substr* (downcase tag) 1))
                        (intern (downcase tag)
				;; 'autowrap
				))))))

(cl-defmethod parse-type (form (_tag (eql :struct)))
  (make-record-ref form))

(cl-defmethod parse-type (form (_tag (eql :union)))
  (make-record-ref form))

(cl-defmethod parse-type (form (_tag (eql :enum)))
  (make-record-ref form))

(cl-defmethod parse-type (form (_tag (eql :pointer)))
  (let ((type (alist-get 'type form)))
    (let ((type-tag (alist-get 'tag type)))
      (cond
        ((or (string= ":char" type-tag)
             (string= ":unsigned-char" type-tag))
         '((:string)))
        (t `((:pointer ,@(parse-type type type-tag))))))))

(cl-defmethod parse-type (_form (_tag (eql :function-pointer)))
  '((:pointer (:void))))

(cl-defmethod parse-type (_form (_tag (eql :signed-char)))
  '(:char))

(cl-defmethod parse-type (_form (_tag (eql :long-double)))
  '(long-double))

(cl-defmethod parse-type (form (_tag (eql :_bool)))
  (cl-case (alist-get 'bit-size form)
    ((8 nil) '(:unsigned-char))
    (32 '(:unsigned-int))))

(cl-defmethod parse-type (form (_tag (eql :array)))
  (let ((type (alist-get 'type form))
	(size (alist-get 'size form)))
    `((:array ,@(parse-type type (alist-get 'tag type)) ,size))))

(cl-defmethod parse-type (form (_tag (eql :bitfield)))
  (let ((type (alist-get 'type form))
	(width (alist-get 'width form)))
    `(,@(parse-type type (alist-get 'tag type))
      :bitfield-p t
      :bit-width ,width)))

(defun make-foreign-record-name (form type)
  (let ((name (alist-get 'name form))
	(id (alist-get 'id form)))
    (let ((size (list :bit-size (alist-get 'bit-size form)
                      :bit-alignment (alist-get 'bit-alignment form)))
          (symbol-type (cl-ecase type (:struct :cstruct) (:union :cunion))))
      (if t ;;XXX (anonymous-p form)
          (cl-list* nil :id id size)
          (let ((sym (foreign-type-symbol name symbol-type *package*)))
            (cl-pushnew `(,type (,sym)) *foreign-record-list* :test #'equal)
            (list* sym size))))))

(cl-defmethod parse-type (form (_tag (eql 'struct)))
  (let ((fields (alist-get 'fields form)))
    `((struct ,(make-foreign-record-name form :struct)
              ,@(parse-fields fields)))))

(cl-defmethod parse-type (form (_tag (eql 'union)))
  (let ((fields (alist-get 'fields form)))
    `((union ,(make-foreign-record-name form :union)
             ,@(parse-fields fields)))))

(cl-defmethod parse-type (form (_tag (eql 'enum)))
  (let ((name (alist-get 'name form))
	(id (alist-get 'id form))
	(fields (alist-get 'fields form)))
    `((enum ,(if t ;;XXX (anonymous-p form)
                 (list nil :id id)
               (foreign-type-symbol name :cenum *package*))
            ,@(parse-enum-fields fields)))))

(cl-defgeneric parse-form (form tag &key &allow-other-keys)
  (:documentation "Parse FORM tagged as TAG; specialize on \(eql `symbol')"))

(cl-defmethod parse-form :around (form tag &rest keys &key &allow-other-keys)
;;  (warn "FOO")
  (cl-etypecase tag
    (symbol (cl-call-next-method))
    (string
     (apply #'parse-form form (if (eq ?\: (aref tag 0))
                                  (make-keyword (substr* (downcase tag) 1))
                                  (intern (downcase tag)
					  ;; 'autowrap
					  ))
            keys))))

(cl-defmethod parse-form (form tag &key &allow-other-keys)
  (warn "Unhandled form: %s for input:\n  %s" tag form))

(cl-defmethod parse-form (form (_tag (eql 'typedef)) &key &allow-other-keys)
  (let* ((name (alist-get 'name form))
	 (type (alist-get 'type form))
	 (sym (foreign-type-symbol name :ctype *package*)))
    (if (pointer*-to-record-form-p type)
	(cl-pushnew sym *foreign-record-list* :test #'equal)
      (if (pointer-alias-form-p type)
	  (cl-pushnew sym *foreign-alias-list* :test #'equal)
	(cl-pushnew sym *foreign-other-exports-list*)))
    `(define-foreign-alias
      ',sym
      ',@(parse-type type (alist-get 'tag type)))))

(when nil
(cl-defmacro alist-bind ((&rest vars) alist &body body)
  `(let (,@(mapcar (lambda (x)
                     (if (consp x)
                         `(,(car x) (alist-get ,(cadr x) ,alist))
                       `(,x (alist-get ',x ,alist))))
                   vars))
     ,@body))
(alist-bind (name type bit-size bit-offset bit-alignment) field nil)
)

(cl-defun parse-fields (fields &optional (field-type :cfield))
  (cl-loop for field in fields
        collect
	(let ((name (alist-get 'name field)) (type (alist-get 'type field))
	      (bit-size (alist-get 'bit-size field))
	      (bit-offset (alist-get 'bit-offset field))
	      (bit-alignment (alist-get 'bit-alignment field)))
          (let ((symbol (foreign-type-symbol name field-type *package*)))
            (cl-list* symbol
                   `(,@(parse-type type (alist-get 'tag type))
                     ,@(when (eq field-type :cfield)
                         `(:bit-size ,bit-size
                           :bit-offset ,bit-offset
                           :bit-alignment ,bit-alignment))))))))

(defun cl-string (s)
  (cl-etypecase s
    (symbol (symbol-name s))
    (character (string s))
    (string s)))


(cl-defun find-prefix (list &key (pred #'cl-string))
  (let* ((sorted-fields (sort (cl-map 'vector pred list) #'string<))
         (first (elt sorted-fields 0))
         (last (when (> (length sorted-fields) 1)
                 (elt sorted-fields (1- (length sorted-fields))))))
    (if (and first last)
        (let ((n (cl-mismatch first last)))
          (cond
	   ;; Never allow blanks
           ((and n (> n 0) (= n (length first))) (1- n))
           (n n)
           (t 0)))
      0)))

(defun parse-enum-fields (fields)
  (let* ((type-symbol-fields
          (cl-map 'vector
               (lambda (x)
                 (symbol-name
                  (foreign-type-symbol (alist-get 'name x)
                                       :cenumfield
                                       *package*)))
               fields))
         (prefix-end (find-prefix type-symbol-fields)))
    (cl-loop for field in fields
             as name = (foreign-type-symbol (alist-get 'name field)
                                            :cenumfield *package*)
             as string = (symbol-name name)
             as truncated = (if (foreign-symbol-exception-p
				 (alist-get 'name field))
				string
                              (substr* string prefix-end))
             collect (cons (intern truncated
                                   (symbol-package name)
				   )
                           (alist-get 'value field)))))

(defun parse-enum-to-const (fields)
  (cl-loop for field in fields
           as name = (alist-get 'name field)
           collect (maybe-add-constant name (alist-get 'value field))
           into constants
           finally (return (cl-remove-if #'null constants))))

(cl-defmethod parse-form (form (_tag (eql 'struct)) &key &allow-other-keys)
  (let ((name (alist-get 'name form)) (fields (alist-get 'fields form)))
    (let ((sym (foreign-type-symbol name :cstruct *package*)))
      (let ((cstruct-fields (parse-fields fields)))
        (when (symbol-package sym)
          (cl-pushnew `(:struct (,sym)) *foreign-record-list*
                      :test #'equal))
        `(define-foreign-record ',sym :struct
				,(alist-get 'bit-size form)
				,(alist-get 'bit-alignment form)
				',cstruct-fields)))))

(cl-defmethod parse-form (form (_tag (eql 'union)) &key &allow-other-keys)
  (let ((name (alist-get 'name form)) (fields (alist-get 'fields form)))
    (let ((sym (foreign-type-symbol name :cunion *package*)))
      (let ((cunion-fields (parse-fields fields)))
	(when (symbol-package sym)
          (cl-pushnew `(:union (,sym)) *foreign-record-list*
                      :test #'equal))
	`(define-foreign-record ',sym :union
				,(alist-get 'bit-size form)
				,(alist-get 'bit-alignment form)
				',cunion-fields)))))

(cl-defmethod parse-form (form (_tag (eql 'enum)) &key &allow-other-keys)
  (let ((name (alist-get 'name form)) (id (alist-get 'id form))
	(fields (alist-get 'fields form)))
    (let ((sym (foreign-type-symbol name :cenum *package*)))
      (when (symbol-package sym)
        (cl-pushnew sym *foreign-other-exports-list*))
      `(progn
         ,@(parse-enum-to-const fields)
         (define-foreign-enum ',sym ,id ',(parse-enum-fields fields))))))

(cl-defmethod parse-form (form (_tag (eql 'function)) &key &allow-other-keys)
  (let ((name (alist-get 'name form)) (inline (alist-get 'inline form))
     (parameters (alist-get 'parameters form))
     (return-type (alist-get 'return-type form))
     (variadic (alist-get 'variadic form)))
    (unless inline
      (let ((sym (foreign-type-symbol name :cfun *package*)))
;;	(setq $param parameters)
        (let ((cfun-fields (parse-fields parameters :cparam)))
          (push sym *foreign-function-list*)
          `(define-foreign-function '(,sym ,name
                                      ,@(when variadic '(:variadic-p t)))
               ',@(parse-type return-type (alist-get 'tag return-type))
             ',cfun-fields))))))

(cl-defmethod parse-form (form (_tag (eql 'const)) &key &allow-other-keys)
  (let ((name (alist-get 'name form)) (value (alist-get 'value form)))
    (maybe-add-constant name value)))

(cl-defmethod parse-form (form (_tag (eql 'extern)) &key &allow-other-keys)
  (let ((name (alist-get 'name form)) (type (alist-get 'type form)))
    (let ((sym (foreign-type-symbol name :cextern *package*)))
      (push sym *foreign-extern-list*)
      `(define-foreign-extern ',sym ,name ',@(parse-type type (alist-get 'tag type))))))

(defun read-json (file)
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer
     :array-type 'list :null-object :null :false-object nil :object-type 'alist)))

(cl-defun read-parse-forms (in-spec &optional
				    exclude-definitions exclude-sources
				    include-definitions include-sources)
  (cl-loop for form in (read-json in-spec)
           as name = (alist-get 'name form)
           as location = (alist-get 'location form)
           unless
           (and (or (included-p name exclude-definitions)
                    (and (included-p location exclude-sources)
			 (not (included-p name include-definitions))))
		(not (or (included-p name include-definitions)
			 (and (included-p location include-sources)
                              (not (included-p name exclude-definitions))))))
           collect (parse-form form (alist-get 'tag form)) into forms
           finally (return (cl-remove-if #'null forms))))

(defun dump-forms (forms file)
  (with-temp-buffer
    (cl-loop for form in forms
	     do (insert (format "%S\n" form)))
      (write-region (point-min) (point-max) file)))

(defun dump-parse-defs (infile outfile)
  (let ((all-forms (collecting-symbols
	      (read-parse-forms infile))))
    (dump-forms all-forms outfile)))

(when nil
(call-process "c2ffi" nil '(:file "/dev/shm/2.json") nil "/usr/include/math.h")
(dump-parse-defs "/dev/shm/2.json" "/dev/shm/r")
)

(defun type-keyord-or-bust (type-spec)
  (cl-etypecase type-spec
    (keyword type-spec)
    (cons (cl-ecase (car type-spec)
	    (:string :pointer)
	    (:pointer :pointer)))))

(defvar $current-library nil)

(defun define-foreign-function (name return-type args)
  (list 'define-ffi-function (car name) (cadr name)
	return-type
	(mapcar 'type-keyord-or-bust (mapcar 'cadr args))
	$current-library))

(when nil
(define-ffi-library math "/tmp/libm.so")
(ffi--dlsym "sqrt" (math))
(setq $current-library 'math)
(define-foreign-function (sqrt "sqrt") :double ((__x :double)))
;; grep sqrt /dev/shm/r
(eval (define-foreign-function '(sqrt "sqrt") ':double '((__x :double))))
(sqrt 2.0))

