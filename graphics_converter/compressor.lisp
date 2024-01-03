(defpackage #:mercury
  (:use #:cl))

(in-package #:mercury)

(deftype octet () '(unsigned-byte 8))

(defclass graphics-header ()
  ((word :accessor bits :initarg :word)
   (stream-position :accessor stream-position :initform 0)
   (index :accessor index :initform 0)))

(defparameter *decompressed-stream* nil)
(defparameter *current-header* nil)
(defparameter *compressed-stream* nil)
(defparameter *file-stream* nil)

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
      (let ((iter (- iteration-count 1))
            (source (dpb 0 (byte 3 0)
                         (dpb 7 (byte 3 13) (+ #x10000 start-vector)))))
        (push-bit-to-header 0)
        (push-bit-to-header 1)
        (store-word source *compressed-stream*)
        (vector-push-extend iter *compressed-stream*))))

(defun push-end-to-stream ()
  (push-bit-to-header 0)
  (push-bit-to-header 1)
  (store-word 0 *compressed-stream*)
  (replace-word (bits *current-header*) *compressed-stream* (stream-position *current-header*)))

(defun take (n list &optional (result '()))
  (if (= n 0)
      (reverse result)
      (take (- n 1) (rest list) (cons (first list) result))))

(defun drop (n list)
  (if (= n 0)
      list
      (drop (- n 1) (rest list))))

(defun repeat-count (repeat-bytes tile &optional (result -1))
  "Returns an integer for the number of times it repeats."
  (if (equal repeat-bytes (take (length repeat-bytes) tile))
      (repeat-count repeat-bytes (drop (length repeat-bytes) tile) (+ result 1))
      (min result 255)))

(defun look-for-copies (current-tile)
  "Returns a list of (width count) of the width with the most copies."
  (reduce (lambda (current-max new-value)
            (if (>= (second new-value) (second current-max))
                new-value
                current-max))
          (loop for i from 1 to 4
                for bytes = (take i current-tile)
                collect (list i (repeat-count bytes current-tile)))
          :initial-value '(0 0)))

(defun calculate-next-action (current-tile)
  (let ((best-copy (look-for-copies current-tile)))
    (cond ((> (second best-copy) 2)
          (progn
            (dotimes (iter (first best-copy)) (push-byte-to-stream (nth iter current-tile)))
            (copy-bytes-to-stream (- (first best-copy)) (second best-copy))
            (drop (* (first best-copy) (+ 1 (second best-copy))) current-tile)))

      ((not (null current-tile))
      (progn
        (push-byte-to-stream (first current-tile))
        (rest current-tile)))

      ('otherwise
       (progn
         (push-end-to-stream)
         nil)))))

(defun main-loop (graphics-data)
  (let ((remaining-data (calculate-next-action graphics-data)))
    (when remaining-data
        (main-loop remaining-data))))

(defun compress-file (filename destination-filename)
  (let ((decompressed-stream (with-open-file (in filename
                                                 :element-type '(unsigned-byte 8))
                               (loop for byte = (read-byte in nil)
                                     while byte
                                     collect byte))))
        (make-compressed-stream)
        (main-loop decompressed-stream)
        (with-open-file (output destination-filename
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :overwrite
                                :if-does-not-exist :create)
          (loop for i from 0 to (length *compressed-stream*)
                do (write-byte i output)))))

; Test
; Big concern is I think my iteration bytes + count is off by one potentially
; need to check, and if the iter_bytes is wrong adjust in copy-bytes-to-stream
(close *file-stream*)
(setf *file-stream* (open "test.dc" :element-type 'octet))
(make-compressed-stream)

(setf *decompressed-stream*
    (loop for byte = (read-byte *file-stream* nil)
        while byte
        collect byte))

(compress-file "test.dc" "recompressed")
