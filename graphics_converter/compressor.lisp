(defpackage #:mercury
  (:use #:cl))

(in-package #:mercury)

(deftype octet () '(unsigned-byte 8))

(defclass graphics-header ()
  ((word :accessor bits :initarg :word)
   (stream-position :accessor stream-position :initform 0)
   (index :accessor index :initform 0)))

(defparameter *current-header* nil)
(defparameter *compressed-stream* nil)
(defparameter *file-stream* (open "test.decompressed"
                                  :element-type 'octet))

(defun make-compressed-stream ()
  (setf *current-header* (make-instance 'graphics-header :word #x0000)
        *compressed-stream* (make-array 0 :element-type '(vector octet)
                                          :adjustable t
                                          :fill-pointer 0))
  (store-word #x0000 *compressed-stream*))

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

(defun replace-word (word vector index)
  "Replaces the segment at index with the new word"
  (setf (aref vector index) (ldb (byte 8 0) word)
        (aref vector (+ 1 index)) (ldb (byte 8 8) word)))

(defun push-bit-to-header (bit)
  (setf (bits *current-header*)
        (dpb bit (byte 1 (index *current-header*)) (bits *current-header*)))
  (incf (index *current-header*))
  (if (> (index *current-header*) 15)
      (let ((new-header (make-instance 'graphics-header :word #x0000))
            (new-position (length *compressed-stream*)))
        (replace-word (bits *current-header*) *compressed-stream* (stream-position *current-header*))
        (store-word #x0000 *compressed-stream*)
        (setf *current-header* new-header
              (stream-position new-header) new-position))))

(defun push-byte-to-stream (byte)
  (push-bit-to-header 1)
  (vector-push-extend byte *compressed-stream*))

(defun copy-bytes-to-stream (start-vector iteration-count)
  (if (< iteration-count 6)
      (let ((iter (- iteration-count 2))
            (source (ldb (byte 8 0)(+ #x100 start-vector))))
        (push-bit-to-header 0)
        (push-bit-to-header 0)
        (push-bit-to-header (ldb (byte 1 1) iter))
        (push-bit-to-header (ldb (byte 1 0) iter))
        (vector-push-extend source *compressed-stream*))
      ;TODO: write bulk copy
      nil))

(defun push-end-to-stream ()
  (push-bit-to-header 0)
  (push-bit-to-header 1)
  (store-word 0 *compressed-stream*)
  (replace-word (bits *current-header*) *compressed-stream* (stream-position *current-header*)))

; Test
(close *file-stream*)
(setf *file-stream (open "test.decompressed" :element-type octet))
(make-compressed-stream)
