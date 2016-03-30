(in-package :cl-user)

(defpackage bit-wise
  (:use :cl
        :cl-ppcre
        :rmatch)
  (:export :byte->bits
           :bits->byte
           :bits->bytes
           :bytes->bits
           :stream->bits
           :file->bits
           :emit-hex
           :emit-binary
           :slip-l
           :slip-r
           :list-contains
           :list-contains-all
           :invert
           :stream-bits
           :as-bin
           :as-hex
           :write-slip
           :inbits))

(in-package :bit-wise)

(defun byte->bits (val)
  (coerce (loop for i from 7 downto 0
             collect (ldb (byte 1 i) val)) 'bit-vector))

(defun bits->byte (bits &optional (mult 128))
  (if bits
      (+ (* mult (car bits)) (bits->byte (cdr bits) (/ mult 2)))
      0))

(defun bits->bytes (bits)
  (when (> (length bits) 0)
    (cons (bits->byte (loop for i from 0 to 7 collect (elt bits i)))
          (bits->bytes (subseq bits 8)))))

(defun bytes->bits (b)
  (when b
    (concatenate 'bit-vector (byte->bits (car b)) (bytes->bits (cdr b)))))

(defun stream->bits (stream &optional (v #*))
  (let ((b (read-byte stream nil nil)))
    (if b
        (stream->bits stream (concatenate 'bit-vector v (byte->bits b)))
        v)))


(defun seq->bits (seq bits)
  (loop for x across seq
        as j = 0 then (+ 1 j)
        do (replace bits (byte->bits (elt seq j)) :start1 (* j 8)))
  bits)
  
(defun file->bits (name)
  (with-open-file (stream name :element-type 'unsigned-byte)
                  (let* ((len (file-length stream))
                         (bits (make-sequence 'bit-vector (* len 8)))
                         (buf (make-sequence '(vector unsigned-byte) len)))
                    (read-sequence buf stream)
                    (seq->bits buf bits)
                    bits)))

(defun emit-hex (bits &optional (stream *standard-output*) (offset 0))
  (when bits
    (destructuring-bind (a b c d e f g h &rest r) (padright bits)
      (let ((int (bits->byte (list a b c d e f g h))))
        (when (< int 16)
          (princ "0" stream))
        (princ (write-to-string int :base 16) stream)
        (when (oddp offset)
          (princ " " stream))
        (if (> offset 14)
            (progn
              (terpri stream)
              (emit-hex r stream 0))
            (emit-hex r stream (+ 1 offset)))))))

(defun emit-binary (bits &optional (stream *standard-output*) (offset 0))
  (when bits
    (princ (car bits) stream)
    (princ " " stream)
    (if (> offset 39)
        (progn
          (terpri stream)
          (emit-binary (cdr bits) stream 0))
        (emit-binary (cdr bits) stream (+ offset 1)))))

(defun padright (bits &optional (val 0))
  (let* ((len (length bits))
         (p (mod len 8)))
    (if (> p 0)
        (concatenate 'bit-vector bits
                     (coerce (make-list (- 8 p) :initial-element val) 'bit-vector))
        bits)))

(defun slip-l (n bits &optional (pad 0))
  (padright (subseq bits n) pad))

(defun slip-r (n bits &optional (pad 0))
  (reverse (slip-l n (reverse bits) pad)))

(defun/match list-compare (l1 l2)
  ((#* _) t)
  ((_ #*) nil)
  ((a b) (and (equal (elt a 0) (elt b 0)) (list-compare (subseq a 1) (subseq b 1)))))

(defun list-contains (sl bl)
  (let ((s (search sl bl)))
    (if s
        (values t s)
      (values nil 0))))

(defun list-contains-all (sl bl)
  (loop for oo = 0 then (+ 1 off)
        as off = (search sl bl :start2 oo)
        when off collecting off into coll
        do (unless off (loop-finish))
        finally (return coll)))

(defun invert (bits)
  (bit-not bits))

(defun stream-bits (s bits)
  (loop for b in (bits->bytes bits)
     do (write-byte b s)))

(defun as-bin (p)
  (coerce (loop for x across p
             collect (digit-char-p x)) 'bit-vector))

(defun as-hex (p)
  (labels ((rebyte (l)
             (when l
               (destructuring-bind (b1 b2 &rest r) l
                 (cons (+ (* b1 16) b2) (rebyte r))))))
    (bytes->bits (rebyte (loop for x across (string-downcase p)
                            collect (digit-char-p x 16))))))

(defun write-slip (n s bits pad slip)
  (loop for b in (bits->bytes (funcall slip (parse-integer n) bits pad))
     do (write-byte b s)))

(defun inbits ()
  (stream->bits *standard-input*))
