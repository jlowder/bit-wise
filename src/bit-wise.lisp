(in-package :cl-user)

(defpackage bit-wise
  (:use :cl :rmatch)
  (:export :byte->bits
           :bits->byte
           :bits->bytes
           :bytes->bits
           :stream->bits
           :file->bits
           :bv->bits
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
           :inbits
           :blit
	   :xor-bits
           :prbs))

(in-package :bit-wise)

(defun byte->bits (val)
  (coerce (loop for i from 7 downto 0
             collect (ldb (byte 1 i) val)) 'bit-vector))

(defun bits->byte (bits &optional (mult 128))
  (if bits
      (+ (* mult (car bits)) (bits->byte (cdr bits) (/ mult 2)))
      0))

(defun bits->bytes (bits)
  (loop for x to (- (length bits) 8) by 8
     as b = (subseq bits x (+ x 8))
     collect (bits->byte (coerce b 'list))))

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

(defun bv->bits (bv)
  (when (not (equal #* bv))
    (append (coerce (subseq bv 0 8) 'list) (bv->bits (subseq bv 8)))))

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

(defun list-contains (sl bl)
  "see if a small list occurs within a bigger list. return boolean result and location of occurrence."
  (let ((s (search sl bl)))
    (if s
        (values t s)
      (values nil 0))))

(defun list-contains-all (sl bl)
  "return a list of all occurrences of sl within bl"
  (loop for oo = 0 then (+ 1 off)
        as off = (search sl bl :start2 oo)
        when off collecting off into coll
        do (unless off (loop-finish))
        finally (return coll)))

(defun invert (bits)
  (bit-not bits))

(defun stream-bits (s bits)
  (loop for b in (bits->bytes (padright bits))
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

(defun blit (l p)
  "make a vector of length l, repeat p into it enough times to fill it"
  (let ((bits (make-sequence 'bit-vector l))
	(pl (length p)))
    (loop for x from 0 to l by pl do (replace bits p :start1 x))
    bits))

(defun xor-bits (sl bl)
  "xor a small list into a bigger list"
  (bit-xor (blit (length bl) sl) bl))

(defun bv2int (b &optional (m 1) (a 0))
  (if (equal #* b)
      a
      (let ((e (- (length b) 1)))
        (bv2int (subseq b 0 e) (* 2 m) (+ a (* m (elt b e)))))))

(defun bitbv (n)
  (if (eq 0 n)
      #*0
      #*1))

(defun prbs-n (bv taps)
  (flet ((newbit (bv)
           (match taps
             ((t1 t2) (logxor (elt bv t1) (elt bv t2)))
             ((t1 t2 t3 t4) (logxor (elt bv t1) (elt bv t2)
                                    (elt bv t3) (elt bv t4))))))
    (concatenate 'bit-vector
                 (subseq bv 1)
                 (bitbv (newbit bv)))))

(defun gen-prbs (init n taps)
    (loop repeat (- (expt 2 n) 1)
     as x = init then (prbs-n x taps)
     collect (bv2int x)))

(defun gen-prbs-bv (init n t1 t2 &rest taps)
  (let* ((len (* n (- (expt 2 n) 1)))
         (bv (make-sequence 'bit-vector len)))
    (loop 
       for y from 0 to len by n
       as x = init then (prbs-n x (append (list t1 t2) taps))
       do (replace bv x :start1 y))
    bv))
       
(defun/match prbs (n)
  ((3) (gen-prbs-bv #*010 n 0 1))
  ((4) (gen-prbs-bv #*0010 n 0 1))
  ((5) (gen-prbs-bv #*00010 n 0 2))
  ((6) (gen-prbs-bv #*000010 n 0 1))
  ((7) (gen-prbs-bv #*0000010 n 0 1))
  ((8) (gen-prbs-bv #*00000010 n 0 2 3 4))
  ((9) (gen-prbs-bv #*000000010 n 0 4))
  ((10) (gen-prbs-bv #*0000000010 n 0 3))
  ((11) (gen-prbs-bv #*00000000010 n 0 2))
  ((12) (gen-prbs-bv #*000000000010 n 0 1 2 8))
  ((13) (gen-prbs-bv #*0000000000010 n 0 1 2 5))
  ((14) (gen-prbs-bv #*00000000000010 n 0 1 2 12))
  ((15) (gen-prbs-bv #*000000000000010 n 0 1))
  ((16) (gen-prbs-bv #*0000000000000010 n 0 1 3 12))
  ((17) (gen-prbs-bv #*00000000000000010 n 0 3))
  ((18) (gen-prbs-bv #*000000000000000010 n 0 7))
  ((19) (gen-prbs-bv #*0000000000000000010 n 0 1 2 5))
  ((20) (gen-prbs-bv #*00000000000000000010 n 0 17))
  ((21) (gen-prbs-bv #*000000000000000000010 n 0 2))
  ((22) (gen-prbs-bv #*0000000000000000000010 n 0 1))
  ((23) (gen-prbs-bv #*00000000000000000000010 n 0 5))
  ((_) nil))
