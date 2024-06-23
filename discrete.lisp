(in-package :clf-math)

(defun product (l)
  "Multiplies a list of numbers together"
  (apply #'* l))

(defun sum (l)
  "Sums a list of numbers"
  (apply #'+ l))

(defun modular-exponent (x n m)
  "Returns x^n mod m"
  (flet ((z () (modular-exponent
		(mod (* x x) m)
		(ash n -1) m)))
    (cond
      ((= m 1) 0)
      ((= n 0) 1)
      ((= (logand n 1) 1)
       (mod (* x (z)) m))
      (t (z)))))

(defun extended-euclid (a b)
  "Returns the result of the extended euclid algorithm applied to a and b"

  (labels ((new-cdr (q n) (- (car n) (* q (cdr n))))
	   (shift (q n) (cons (cdr n) (new-cdr q n)))
	   (extended-euclid-rec (r x y)
	     (if (= (cdr r) 0)
		 (list r x y)
		 (let ((q (floor (/ (car r) (cdr r)))))
		   (extended-euclid-rec (shift q r) (shift q x) (shift q y))))))
    (let ((r a) (rp b)
	  (x 1) (xp 0)
	  (y 0) (yp 1))
      (extended-euclid-rec (cons r rp) (cons x xp) (cons y yp)))))
    
(defun modular-inverse (x mod)
  "Returns the modular inverse of x modulus mod"
  (cond
    ((or (= x 0) (<= mod 0)) 0)
    ((or (< x 2) (< mod 2)) 1)
    (t (let ((l (extended-euclid x mod)))
	 (cond
	   ((not (= (caar l) 1)) l)
	   ((< (caadr l) 0) (+ (caadr l) mod))
	   (t (caadr l)))))))

(defun fib (n)
  "Calculates the nth fibonacci number using matrix exponentiation"

  (labels ((sq-matrix-multiply (A B)
	     (let* ((dims (array-dimensions A))
		    (C (make-array dims :initial-element 0))
		    (rows (car dims))
		    (cols (cadr dims)))
	       (dotimes (i rows)
		 (dotimes (j cols)
		   (dotimes (k rows)
		     (setf (aref C i j) (+ (aref C i j) (* (aref A i k) (aref B k j)))))))
	       C))
	   (sq-matrix-power (A n)
	     (let ((size (car (array-dimensions A)))
		   (R (make-array (array-dimensions A) :initial-element 0))
		   (B (make-array (array-dimensions A) :initial-element 0)))
	       (dotimes (i size)
		 (setf (aref R i i) 1))
	       (dotimes (i size)
		 (dotimes (j size)
		   (setf (aref B i j) (aref A i j))))
	       (do ((m n m))
		   ((= m 0) R)
		 (print m)
		 (if (= (logand m 1) 1)
		     (progn (setf m (1- m))
			    (setf R (sq-matrix-multiply R B)))
		     (progn (setf m (ash m -1))
			    (setf B (sq-matrix-multiply B B))))))))
    (let ((initial-matrix #2A((1 1) (1 0))))
      (aref (sq-matrix-power initial-matrix n) 0 1))))

(defun is-prime-fermat-fibonacci (n)
  (when (> n 1)
    (or
     ;; fermat's little theorem
     (= (modular-exponent 2 (- n 1) n) 1)
     ;; fibonacci test
     (= (mod (fib (+ n 1)) n) 0))))

(defun divides (d n)
  (= (mod n d) 0))

(defun is-prime-iter (n)
  (when (> n 1)
    (do ((i 2 (1+ i))
	 (prime t prime))
	((or (not prime) (= i (floor (sqrt n)))) prime)
      (when (divides i n)
	(setf prime nil)))))

(defun factorial-wheel (n)
  "Returns the prime factors composing n"
  
  (labels ((factorial-wheel-rec (n k inc)
	   (if (> (expt k 2) n)
	       (cons n nil)
	       (if (divides k n)
		   (cons k (factorial-wheel-rec (/ n k) k inc))
		   (factorial-wheel-rec n (+ k (car inc)) (cdr inc))))))
    (let ((inc (list 1 2 2 4 2 4 2 4 6 2 6)))
      (setf (cdr (last inc)) (cdddr inc))
      (if (is-prime-iter n)
	  nil
	  (factorial-wheel-rec n 2 inc)))))
