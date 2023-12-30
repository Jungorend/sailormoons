(defpackage #:jupiter
  (:use #:cl))

(in-package #:jupiter)

(defun load-word (stream)
  (let ((lower-byte (read-byte stream))
        (higher-byte (read-byte stream)))
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
  (vector-push-extend (read-byte stream) vector))

(defclass graphics-header ()
  ((word :accessor bits :initarg :word)
   (index :accessor index :initform 0)))

(defun pull-header (stream)
  (make-instance 'graphics-header :word (load-word stream)))

(defun pop-header-bit (stream header)
  (let ((bit (ldb (byte 1 (index header)) (bits header))))
    (incf (index header))
    (when (eql 16 (index header))
      (setf (bits header) (load-word stream)
            (index header) 0))
    bit))

(defun bulk-copy (starting-index count decompressed-data)
  (loop for i from starting-index to (+ starting-index count)
        do (vector-push-extend (aref decompressed-data i) decompressed-data)))

(defun small-copy (stream header decompressed-data)
  (let* ((x (pop-header-bit stream header))
        (y (pop-header-bit stream header))
        (iter-count (+ 2 (dpb x (byte 8 1) y)))
        (source-vector (dpb #xff (byte 8 8) (read-byte stream)))
        (calculated-source (+ (length decompressed-data)
                              (dpb 0 (byte 8 16) source-vector))))
    (bulk-copy calculated-source iter-count decompressed-data)))

(defun large-copy-or-quit (stream decompressed-data)
  (let* ((word (load-word stream))
         (iter-or-quit (ldb (byte 3 0) word))
         (source-vector (dpb 7 (byte 8 13)
                             (dpb (ldb (byte 5 3) word) (byte 8 8)
                                  (ldb (byte 8 0) word))))
         (calculated-source (+ (length decompressed-data)
                               (dpb 0 (byte 8 16) source-vector))))
    (if (eql 0 iter-or-quit)
        (let ((iter-byte (read-byte stream)))
          (case iter-byte
            (0 nil)
            (1 t)
            (otherwise (bulk-copy calculated-source (+ 1 iter-byte) decompressed-data))))
        (bulk-copy calculated-source (+ 2 iter-or-quit) decompressed-data))))

(defun perform-next-action (stream header decompressed-data)
  (if (eql 1 (pop-header-bit stream header))
      (main-loop stream header (copy-byte stream decompressed-data))
      (if (eql 1 (pop-header-bit stream header))
          (large-copy-or-quit stream decompressed-data)
          (small-copy stream header decompressed-data))))

;; Test functions
;; Use to just throw together some data created in compressed-data
(defparameter *compressed-data* (make-array 0 :element-type '(vector (unsigned-byte 8)) :adjustable t :fill-pointer 0))
(defparameter *decompressed-data* (make-array 0 :element-type '(vector (unsigned-byte 8)) :adjustable t :fill-pointer 0))
(defparameter s (open "test.cd" :element-type '(unsigned-byte 8)))
(defparameter *header* (pull-header s))

(loop for i in '(#xad #x6f #x00 #xff #xf8 #x1e #xff #xff #xf8 #x0e #xe0 #xf8 #x0f)
      do (vector-push-extend i *compressed-data*))

(with-open-file (s "test.cd" :direction :output :element-type '(unsigned-byte 8)
                             :if-exists :overwrite :if-does-not-exist :create)
  (loop for b from 0 to (length *compressed-data*)
        do (write-byte (aref *compressed-data* b) s)))
