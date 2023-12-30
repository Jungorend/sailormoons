(defpackage #:jupiter
  (:use #:cl))

(in-package #:jupiter)

(defparameter *compressed-data* (make-array 0 :element-type '(vector (unsigned-byte 8)) :adjustable t :fill-pointer 0))

(defun load-word (stream)
  (let ((higher-byte (read-byte stream))
        (lower-byte (read-byte stream)))
    (dpb higher-byte (byte 8 8) lower-byte)))

(defun store-word (word vector)
  (vector-push-extend (ldb (byte 8 8) word) vector)
  (vector-push-extend (ldb (byte 8 0) word) vector))

(defun load-file (filename offset length)
  (with-open-file (s filename
                     :element-type '(unsigned-byte 8))
    (when (>= (file-length s) (+ offset length))
      (file-position s offset)
      (loop for i from 1 to length
            do (vector-push-extend (read-byte s) *compressed-data*)))))

; Use if single byte needs to be copied
(defun copy-byte (stream vector)
  (vector-push-extend (read-byte stream)))

(defclass graphics-header ()
  ((word :accessor bits :initarg :word)
   (index :accessor index :initform 0)))

(defun pull-header (stream)
  (make-instance 'graphics-header :byte (load-word stream)))

;; Test functions
;; Use to just throw together some data created in compressed-data
(with-open-file (s "test.cd" :direction :output :element-type '(unsigned-byte 8) :if-exists :overwrite)
  (loop for b from 0 to (length *compressed-data*)
        do (write-byte (aref *compressed-data* b) s)))
