;; -*- lexical-binding:t -*-

(require 'cl-macs)

;;madhu 240419 - module-load expands to dlopen() which requires a full
;; path unless the file is in LD_LIBRARY_PATH at program start time.
(let ((default-directory (or (and load-file-name
				  (file-name-directory load-file-name))
			     default-directory)))
  (module-load (expand-file-name  "ffi-module.so")))

(gv-define-simple-setter ffi--mem-ref ffi--mem-set t)

(defmacro define-ffi-library (symbol name)
  (let ((library (cl-gensym)))
    (set library nil)
    `(defun ,symbol ()
       (or ,library
	   (setq ,library (ffi--dlopen ,name))))))

(defmacro define-ffi-function (name c-name return-type arg-types library)
  (let* (
	 ;; Turn variable references into actual types; while keeping
	 ;; keywords the same.
	 (arg-types (mapcar #'symbol-value arg-types))
	 (arg-names (mapcar (lambda (_ignore) (cl-gensym)) arg-types))
	 (arg-types (vconcat arg-types))
	 (function (cl-gensym))
	 (cif (ffi--prep-cif (symbol-value return-type) arg-types)))
    (set function nil)
    `(defun ,name (,@arg-names)
       (unless ,function
	 (setq ,function (ffi--dlsym ,c-name (,library))))
       ;; FIXME do we even need a separate prep?
       (ffi--call ,cif ,function ,@arg-names))))

(defun ffi-lambda (function-pointer return-type arg-types)
  (let* ((cif (ffi--prep-cif return-type (vconcat arg-types))))
    (lambda (&rest args)		; lame
      (apply #'ffi--call cif function-pointer args))))

(defsubst ffi--align (offset align)
  (+ offset (mod (- align (mod offset align)) align)))

(defun ffi--lay-out-struct (types)
  (let ((offset 0))
    (mapcar (lambda (this-type)
	      (setf offset (ffi--align offset
				       (ffi--type-alignment this-type)))
	      (let ((here offset))
		(cl-incf offset (ffi--type-size this-type))
		here))
	    types)))

(defun ffi--struct-union-helper (name slots definer-function layout-function)
  (cl-assert (symbolp name))
  (let* ((docstring (if (stringp (car slots))
			(pop slots)))
	 (conc-name (concat (symbol-name name) "-"))
	 (result-forms ())
	 (field-types (mapcar (lambda (slot)
				(cl-assert (eq (cadr slot) :type))
				(symbol-value (cl-caddr slot)))
			      slots))
	 (the-type (apply definer-function field-types))
	 (field-offsets (funcall layout-function field-types)))
    (push `(defvar ,name ,the-type ,docstring)
	  result-forms)
    (cl-mapc
     (lambda (slot type offset)
       (let ((getter-name (intern (concat conc-name
					  (symbol-name (car slot)))))
	     (offsetter (if (> offset 0)
			    `(ffi-pointer+ object ,offset)
			  'object)))
	 ;; One benefit of using defsubst here is that we don't have
	 ;; to provide a GV setter.
	 (push `(cl-defsubst ,getter-name (object)
		  (ffi--mem-ref ,offsetter ,type))
	       result-forms)))
     slots field-types field-offsets)
    (cons 'progn (nreverse result-forms))))

(defmacro define-ffi-struct (name &rest slots)
  "Like a limited form of `cl-defstruct', but works with foreign objects.

NAME must be a symbol.
Each SLOT must be of the form `(SLOT-NAME :type TYPE)', where
SLOT-NAME is a symbol and TYPE is an FFI type descriptor."
  (ffi--struct-union-helper name slots #'ffi--define-struct
			    #'ffi--lay-out-struct))

(defmacro define-ffi-union (name &rest slots)
  "Like a limited form of `cl-defstruct', but works with foreign objects.

NAME must be a symbol.
Each SLOT must be of the form `(SLOT-NAME :type TYPE)', where
SLOT-NAME is a symbol and TYPE is an FFI type descriptor."
  (ffi--struct-union-helper name slots #'ffi--define-union
			    (lambda (types)
			      (make-list (length types) 0))))

(defmacro define-ffi-array (name type length &optional docstring)
  ;; This is a hack until libffi gives us direct support.
  (let ((type-description
	 (apply #'ffi--define-struct
                (make-list (eval length) (symbol-value type)))))
    `(defvar ,name ,type-description ,docstring)))

(defsubst ffi-aref (array type index)
  (ffi--mem-ref (ffi-pointer+ array (* index (ffi--type-size type))) type))

(defmacro with-ffi-temporary (binding &rest body)
  (declare (indent defun))
  `(let ((,(car binding) (ffi-allocate ,@(cdr binding))))
     (unwind-protect
         (progn ,@body)
       (ffi-free ,(car binding)))))

(defmacro with-ffi-temporaries (bindings &rest body)
  (declare (indent defun))
  (let ((first-binding (car bindings))
	(rest-bindings (cdr bindings)))
    (if rest-bindings
	`(with-ffi-temporary ,first-binding
	   (with-ffi-temporaries ,rest-bindings
	     ,@body))
      `(with-ffi-temporary ,first-binding ,@body))))

(defmacro with-ffi-string (binding &rest body)
  (declare (indent defun))
  `(let ((,(car binding) (ffi-make-c-string ,@(cdr binding))))
     (unwind-protect
         (progn ,@body)
       (ffi-free ,(car binding)))))

(defmacro with-ffi-strings (bindings &rest body)
  (declare (indent defun))
  (let ((first-binding (car bindings))
	(rest-bindings (cdr bindings)))
    (if rest-bindings
	`(with-ffi-string ,first-binding
	   (with-ffi-strings ,rest-bindings
	     ,@body))
      `(with-ffi-string ,first-binding ,@body))))

;; a ltdl handle to the running binary.
(define-ffi-library ffi-self "")

(cl-defun ffi-funcall (c-name &rest args-and-types)
  "ARGS-AND-TYPES ::= { ARG-TYPE ARG-PARAM }* [RETURN-TYPE]."
  (cl-flet ((parse-args-and-types (args)
	      (let (arg-types arg-params return-type (i 0))
		(mapc (lambda (arg)
			(cond ((cl-evenp i) (push arg arg-types))
			      (t (push arg arg-params)))
			(cl-incf i))
		      args)
		(setq return-type (if (cl-oddp i) (pop arg-types) :void))
		(cl-values (nreverse arg-types) (nreverse arg-params)
			   return-type))))
    (cl-multiple-value-bind (arg-types arg-params return-type)
	(parse-args-and-types args-and-types)
      (apply (ffi-lambda (ffi--dlsym c-name (ffi-self))
			 return-type
			 arg-types)
	     arg-params))))

(defun undefine-ffi-library (name)
  (let ((ptr (funcall name)))
    (when (and (user-ptrp ptr) (not (ffi-pointer-null-p ptr)))
      (let ((ret (ffi-funcall "lt_dlclose" :pointer ptr :int)))
	(if (zerop ret)
	    (fset name nil))
	ret))))

(cl-defun ffi-dlsym (string &optional (handler (ffi-self)))
  (ffi--dlsym string handler))

(cl-defun ffi-mem-ref (ptr type &optional (offset 0))
  (ffi--mem-ref (ffi-pointer+ ptr offset) type))

(cl-defun ffi-mem-aref (ptr type &optional (index 0))
  (ffi--mem-ref (ffi-pointer+ ptr (* index (ffi--type-alignment type)))
		type))

(cl-defun ffi-mem-aptr (ptr type &optional (index 0))
  (ffi-pointer+ ptr (* index (ffi--type-alignment type))))

(provide 'ffi)
