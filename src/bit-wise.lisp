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
  (loop for i from 7 downto 0
     collect (ldb (byte 1 i) val)))

(defun bits->byte (bits &optional (mult 128))
  (if bits
      (+ (* mult (car bits)) (bits->byte (cdr bits) (/ mult 2)))
      0))

(defun bits->bytes (bits)
  (when bits
    (destructuring-bind (a b c d e f g h &rest r) (padright bits)
      (cons (bits->byte (list a b c d e f g h)) (bits->bytes r)))))

(defun bytes->bits (b)
  (loop for x in b appending (byte->bits x)))

(defun stream->bits (stream)
  (loop 
   for val = (read-byte stream nil nil)
   until (eq val nil)
   appending (byte->bits val)))

(defun file->bits (name)
  (with-open-file (stream name :element-type 'unsigned-byte)
                  (stream->bits stream)))
                  
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
  (when bits
    (let ((p (mod (length bits) 8)))
      (if (> p 0)
          (append bits (make-list (- 8 p) :initial-element val))
          bits))))

(defun slip-l (n bits &optional (pad 0))
  (padright (subseq bits n) pad))

(defun slip-r (n bits &optional (pad 0))
  (reverse (slip-l n (reverse bits) pad)))

(defun/match list-compare (l1 l2)
  ((nil _) t)
  ((_ nil) nil)
  ((a b) (and (equal (car a) (car b)) (list-compare (cdr a) (cdr b)))))

(defun list-contains (sl bl &optional (off 0))
  (if bl
    (if (list-compare sl bl)
        (values t off)
        (list-contains sl (cdr bl) (+ off 1)))
    (values nil 0)))

(defun list-contains-all (sl bl &optional (off 0))
  (when bl
    (if (list-compare sl bl)
        (cons off (list-contains-all sl (cdr bl) (+ 1 off)))
        (list-contains-all sl (cdr bl) (+ 1 off)))))

(defun invert (bits)
  (mapcar #'(lambda (x) (- 1 x)) bits))

(defun stream-bits (s bits)
  (loop for b in (bits->bytes bits)
     do (write-byte b s)))

(defun as-bin (p)
  (loop for x across p
       collect (digit-char-p x)))

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
