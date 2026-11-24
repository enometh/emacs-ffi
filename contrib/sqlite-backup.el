;; -*- lexical-binding: t; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Nov 12 15:50:00 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
;;; sqlite-backup.el - use the sqlite3 backup api with emacs-ffi only
;;; to backup an already opened sqlite database in emacs.

;;; ffi utils
(cl-defmacro with-foreign-strings (specs &body body)
  (declare (indent 2))
  (let* ((vars (mapcar 'car specs))
	 (vals (mapcar 'cadr specs)))
    `(let (,vars)
       (unwind-protect
	   (progn
	     ,@(cl-loop for var in vars for val in vals
		     collect `(setq ,var (ffi-make-c-string ,val)))
	     ,@body)
	 ,@(cl-loop for var in vars
		 collect `(when ,var (ffi-free ,var)))))))

;; sqlite3 utils
(cl-defmacro with-open-database ((dbvar path &optional readonly disable-uri)
				 &body body)
  (declare (indent 2))
  (cl-check-type dbvar symbol)
  `(let ((,dbvar (sqlite-open ,path ,readonly ,disable-uri)))
     (unwind-protect (progn ,@body)
       (when ,dbvar (sqlite-close ,dbvar)))))

(defvar sqlite-error-codes
  '((:OK . 0)
    (:ERROR . 1)
    (:INTERNAL . 2)
    (:PERM . 3)
    (:ABORT . 4)
    (:BUSY . 5)
    (:LOCKED . 6)
    (:NOMEM . 7)
    (:READONLY . 8)
    (:INTERRUPT . 9)
    (:IOERR . 10)
    (:CORRUPT . 11)
    (:NOTFOUND . 12)
    (:FULL . 13)
    (:CANTOPEN . 14)
    (:PROTOCOL . 15)
    (:EMPTY . 16)
    (:SCHEMA . 17)
    (:TOOBIG . 18)
    (:CONSTRAINT . 19)
    (:MISMATCH . 20)
    (:MISUSE . 21)
    (:NOLFS . 22)
    (:AUTH . 23)
    (:FORMAT . 24)
    (:RANGE . 25)
    (:NOTADB . 26)
    (:ROW . 100)
    (:DONE . 101)))

(defun sqlite-error-code (error-code)
  "Return a keyword"
  (car (rassoc error-code sqlite-error-codes)))

(defun sqlite3-get-dbptr ($db)
  "$DB is lisp SQLITE object, returns a user-pointer to the db slot."
  (with-temp-buffer
    (prin1  $db (current-buffer))
    (goto-char (point-min))
    (and (looking-at "#<sqlite db=0x\\([^ ]+\\) name=\\(.*\\)>")
	 (ffi-make-pointer
	  (string-to-number (buffer-substring (match-beginning 1) (match-end 1))
			    16)))))

;; (ffi-dlsym "sqlite3_open_v2")
;; (ffi-dlsym "sqlite3_backup_init")
;; (ffi-dlsym "sqlite3_backup_step")

(cl-defun sqlite-backup (db path &key (npages -1) (sleep nil) (verbose nil))
  "Backup open database Lisp_Sqlite DB to PATH using the SQLITE C-API. If
NPAGES is a positive number 5, sleep is a non-NIL foat number of
seconds, 0.25 and verbose is non-NIL, mimic the example program
`backupDb' in https://sqlite.org/backup.html."
  (with-foreign-strings ((main "main"))
      (with-open-database (dest path)
	  (let ((p (ffi-funcall "sqlite3_backup_init"
				:pointer (sqlite3-get-dbptr dest)
				:pointer main
				:pointer (sqlite3-get-dbptr db)
				:pointer main
				:pointer))
		error-code)
	    (cl-assert  (not (ffi-pointer-null-p p)))
	    (cl-loop do
		     (progn
		       (setq error-code
			     (ffi-funcall "sqlite3_backup_step"
					  :pointer p
					  :int npages
					  :int))
		       (when verbose
			 (message "sqlite-backup: %d left of %d pages\n"
				  (ffi-funcall "sqlite3_backup_remaining"
					       :pointer p
					       :int)
				  (ffi-funcall "sqlite3_backup_pagecount"
					       :pointer p
					       :int)))
		       (cond ((eql (sqlite-error-code error-code) :DONE)
			      (cl-return))
			     ((member (sqlite-error-code error-code)
				      '(:OK :BUSY :LOCKED))
			      (when sleep  (sleep-for sleep)))
			     (t (cl-return)))))
	    (ffi-funcall "sqlite3_backup_finish" :pointer p :int)
	    (unless (eql (sqlite-error-code error-code) :DONE)
	      (error (format "error %S backing up to %S"
			     (list error-code (sqlite-error-code error-code))
			     path)))))))
