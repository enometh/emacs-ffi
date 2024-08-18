;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-
;;;   Time-stamp: <>
;;;   Touched: Sun Apr 28 11:55:51 AM IST 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
(require 'ffi)

(defvar RTLD_DEFAULT 0)
(defvar RTLD_NEXT -1)
(defvar RTLD_LAZY #x00001)
(defvar RTLD_NOW #x00002)
(defvar RTLD_NOLOAD #x00004)
(defvar RTLD_LOCAL 0)
(defvar RTLD_GLOBAL #x00100)

(defun ffi-check-dlopened (file-name)
  "FILE-NAME is either a path or is a user-ptr to a C string denoting a
path. Returns the handle returned by dlopen(2) as a user-ptr, if it is
already opened by dlsym, and NIL otherwise."
  (cl-flet ((check-dlopened-ptr (ptr)
	      (ffi-funcall "dlopen" :pointer ptr
			   :int (logior RTLD_LAZY RTLD_LOCAL RTLD_NOLOAD)
			   :pointer)))
    (let ((ptr (if (stringp file-name)
		   (with-ffi-string (ptr file-name)
		     (check-dlopened-ptr ptr))
		 (cl-check-type file-name user-ptr)
		 (check-dlopened-ptr file-name))))
      (if (ffi-pointer-null-p ptr)
	  nil
	(cl-assert (= 0 (ffi-funcall "dlclose" :pointer ptr :int)))
	ptr))))

(defvar $RTLD_NEXT (ffi-make-pointer -1))
(defvar $RTLD_DEFAULT (ffi-make-pointer RTLD_DEFAULT))

(provide 'ffi-dlsym-extras)
