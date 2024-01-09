(defpackage #:jupiter
  (:use #:cl))

(in-package #:jupiter)

(defparameter *current-header* nil)
(defparameter *compressed-stream* nil)

(deftype octet () '(unsigned-byte 8))

(defclass graphics-header ()
  ((word :accessor bits :initarg :word)
   (stream-position :accessor stream-position :initform 0) ; only used for compressing
   (index :accessor index :initform 0)))

;; Utility functions
(defun load-word (stream)
  "Returns a word pulled from the stream. Stream is little-endian."
  (let ((lower-byte (read-byte stream))
        (higher-byte (read-byte stream)))
    (logior lower-byte
            (ash higher-byte 8))))

(defun store-word (word vector)
  "Pushes a word little-endian style into vector"
  (vector-push-extend (ldb (byte 8 0) word) vector)
  (vector-push-extend (ldb (byte 8 8) word) vector))

(defun replace-word (word vector index)
  "Replaces the segment at index with the new word"
  (setf (aref vector index) (ldb (byte 8 0) word)
        (aref vector (+ 1 index)) (ldb (byte 8 8) word)))

(defun copy-byte (stream vector)
  "Pushes a single byte into vector"
  (vector-push-extend (read-byte stream) vector))

(defun pop-header-bit (stream header)
  "Returns the next bit from the graphics header, and loads a new one from stream if empty."
  (let ((bit (ldb (byte 1 (index header)) (bits header))))
    (incf (index header))
    (when (eql 16 (index header))
      (setf (bits header) (load-word stream)
            (index header) 0))
    bit))

(defun take (n list &optional (result '()))
  (if (= n 0)
      (reverse result)
      (take (- n 1) (rest list) (cons (first list) result))))

(defun drop (n list)
  (if (= n 0)
      list
      (drop (- n 1) (rest list))))

;; Decompressor functions
(defun pull-header (stream)
  "Loads a word from stream and returns a graphics-header from it"
  (make-instance 'graphics-header :word (load-word stream)))

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

(defun decompress-next-action (stream header decompressed-data)
  (if (eql 1 (pop-header-bit stream header))
      (copy-byte stream decompressed-data)
      (if (eql 1 (pop-header-bit stream header))
          (large-copy-or-quit stream decompressed-data)
          (small-copy stream header decompressed-data))))

(defun decompress-main-loop (stream header decompressed-data)
  (when (decompress-next-action stream header decompressed-data)
    (decompress-main-loop stream header decompressed-data)))

(defun decompress-file (filename destination-filename &optional offset)
  (with-open-file (s filename
                       :element-type '(unsigned-byte 8))
    (let ((decompressed-data (make-array 0
                                       :element-type '(vector (unsigned-byte 8))
                                       :adjustable t
                                       :fill-pointer 0))
          (header (pull-header s)))
      (when offset (file-position s offset))
      (decompress-main-loop s header decompressed-data)
      (with-open-file (out destination-filename
                           :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists :overwrite
                           :if-does-not-exist :create)
        (loop for i from 0 to (- (length decompressed-data) 1)
              do (write-byte (aref decompressed-data i) out))))))

;; Compression functions
(defun make-compressed-stream ()
  (setf *current-header* (make-instance 'graphics-header :word #x0000)
        *compressed-stream* (make-array 0 :element-type '(vector octet)
                                          :adjustable t
                                          :fill-pointer 0))
  (store-word #x0000 *compressed-stream*))

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
            (source (dpb 0 (byte 3 8)
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

(defun repeat-count (repeat-bytes tile &optional (result -1))
  "Returns an integer for the number of times it repeats."
  (if (equal repeat-bytes (take (length repeat-bytes) tile))
      (repeat-count repeat-bytes (drop (length repeat-bytes) tile) (+ result 1))
      result))

(defun look-for-copies (current-tile)
  "Returns a list of (width count) of the width with the most copies."
  (reduce (lambda (current-max new-value)
            (if (>= (second new-value) (second current-max))
                new-value
                current-max))
          (loop for i from 1 to 16
                for bytes = (take i current-tile)
                collect (list i (repeat-count bytes current-tile)))
          :initial-value '(0 0)))

(defun compress-next-action (current-tile)
  (let ((best-copy (look-for-copies current-tile)))
    (cond ((> (second best-copy) 2)
          (progn
            (dotimes (iter (first best-copy)) (push-byte-to-stream (nth iter current-tile)))
            (copy-bytes-to-stream (- (first best-copy)) (* (first best-copy)(second best-copy)))
            (drop (* (first best-copy) (+ 1 (second best-copy))) current-tile)))

      ((not (null current-tile))
      (progn
        (push-byte-to-stream (first current-tile))
        (rest current-tile)))

      ('otherwise
       nil))))

(defun compress-main-loop (graphics-data)
  (let ((remaining-data (compress-next-action graphics-data)))
    (if remaining-data
        (compress-main-loop remaining-data)
        (push-end-to-stream))))

(defun compress-file (filename destination-filename)
  (let ((decompressed-stream (with-open-file (in filename
                                                 :element-type '(unsigned-byte 8))
                               (loop for byte = (read-byte in nil)
                                     while byte
                                     collect byte))))
        (make-compressed-stream)
        (compress-main-loop decompressed-stream)
        (with-open-file (output destination-filename
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :overwrite
                                :if-does-not-exist :create)
          (loop for i from 0 to (- (length *compressed-stream*) 1)
                do (write-byte (aref *compressed-stream* i) output)))))

(defun compress-new-file (original-file graphics-data new-file offset)
  (let ((decompressed-stream (with-open-file (in graphics-data
                                                 :element-type '(unsigned-byte 8))
                               (loop for byte = (read-byte in nil)
                                     while byte
                                     collect byte)))
        (orig-file-stream (with-open-file (in original-file
                                              :element-type '(unsigned-byte 8))
                            (loop for byte = (read-byte in nil)
                                  while byte
                                  collect byte))))
    (with-open-file (output new-file
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :overwrite
                            :if-does-not-exist :create)
      (make-compressed-stream)
      (compress-main-loop decompressed-stream)
      (loop for i from 1 to offset
            do (write-byte (nth (- i 1) orig-file-stream) output))
      (loop for i from 0 to (- (length *compressed-stream*) 1)
            do (write-byte (aref *compressed-stream* i) output))
      (loop for i from (+ offset (length *compressed-stream*)) to (- (length orig-file-stream) 1)
            do (write-byte (nth i orig-file-stream) output)))))
