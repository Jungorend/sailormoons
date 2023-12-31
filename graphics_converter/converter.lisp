(defpackage #:jupiter
  (:use #:cl))

(in-package #:jupiter)

(deftype octet () '(unsigned-byte 8))

(defun load-word (stream)
  "Returns a word pulled from the stream. Stream is little-endian."
  (let ((lower-byte (read-byte stream))
        (higher-byte (read-byte stream)))
    (logior lower-byte
            (ash higher-byte 8))))

(defun store-word (word vector)
  "Pushes a word little-endian style into vector"
  (vector-push-extend (ldb (byte 8 8) word) vector)
  (vector-push-extend (ldb (byte 8 0) word) vector))

(defun load-file (filename offset length)
  (with-open-file (s filename
                     :element-type '(unsigned-byte 8))
    (when (>= (file-length s) (+ offset length))
      (file-position s offset)
      (loop for i from 1 to length
            do (vector-push-extend (read-byte s) *compressed-data*)))))

(defun copy-byte (stream vector)
  "Pushes a single byte into vector"
  (vector-push-extend (read-byte stream) vector))

(defclass graphics-header ()
  ((word :accessor bits :initarg :word)
   (index :accessor index :initform 0)))

(defun pull-header (stream)
  "Loads a word from stream and returns a graphics-header from it"
  (make-instance 'graphics-header :word (load-word stream)))

(defun pop-header-bit (stream header)
  "Returns the next bit from the graphics header, and loads a new one from stream if empty."
  (let ((bit (ldb (byte 1 (index header)) (bits header))))
    (incf (index header))
    (when (eql 16 (index header))
      (setf (bits header) (load-word stream)
            (index header) 0))
    bit))

(defun bulk-copy (starting-index count decompressed-data)
  (loop for i from starting-index to (+ starting-index count)
        do (vector-push-extend (aref decompressed-data i) decompressed-data))
  t)

(defun small-copy (stream header decompressed-data)
  (let* ((x (pop-header-bit stream header))
         (y (pop-header-bit stream header))
         (iter-count (+ 1 (dpb x (byte 8 1) y)))
         (source-vector (dpb #xff (byte 8 8) (read-byte stream)))
         (calculated-source (dpb 0 (byte 8 16)
                                 (+ (length decompressed-data)
                                    source-vector))))
    (bulk-copy calculated-source iter-count decompressed-data)))

(defun large-copy-or-quit (stream decompressed-data)
  (let* ((word (load-word stream))
         (iter-or-quit (ldb (byte 3 8) word))
         (source-vector (dpb 7 (byte 8 13)
                             (dpb (ldb (byte 5 11) word) (byte 8 8)
                                  (ldb (byte 8 0) word))))
         (calculated-source (dpb 0 (byte 8 16)
                                   (+ (length decompressed-data)
                                      source-vector))))
    (if (eql 0 iter-or-quit)
        (let ((iter-byte (read-byte stream)))
          (case iter-byte
            (0 nil)
            (1 t)
            (otherwise (bulk-copy calculated-source iter-byte decompressed-data))))
        (bulk-copy calculated-source (+ 1 iter-or-quit) decompressed-data))))

(defun perform-next-action (stream header decompressed-data)
  (if (eql 1 (pop-header-bit stream header))
      (copy-byte stream decompressed-data)
      (if (eql 1 (pop-header-bit stream header))
          (large-copy-or-quit stream decompressed-data)
          (small-copy stream header decompressed-data))))

(defun main-loop (stream header decompressed-data)
  (when (perform-next-action stream header decompressed-data)
    (main-loop stream header decompressed-data)))

(defun decompress-file (filename destination-filename)
  (with-open-file (s filename
                       :element-type '(unsigned-byte 8))
    (let ((decompressed-data (make-array 0
                                       :element-type '(vector (unsigned-byte 8))
                                       :adjustable t
                                       :fill-pointer 0))
          (header (pull-header s)))
      (main-loop s header decompressed-data)
      (with-open-file (out destination-filename
                           :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists :overwrite
                           :if-does-not-exist :create)
        (loop for i from 0 to (length decompressed-data)
              do (write-byte (aref decompressed-data i) out))))))
