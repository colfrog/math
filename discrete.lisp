(in-package :clf-math)

(defun product (l)
  "Multiplies a list of numbers together"
  (apply #'* l))

(defun sum (l)
  "Sums a list of numbers"
  (apply #'+ l))

(defun modular-exponent (x n m)
  "Returns x^n mod m"
  (defun z () (modular-exponent (mod (* x x) m) (ash n (- 1)) m))
  (cond
    ((= n 0) 1)
    ((= (logand n 1) 1)
     (mod (* x (z)) m))
    (t (z))))

(defun extended-euclid (a b)
  "Returns the result of the extended euclid algorithm applied to a and b"

  (defun extended-euclid-rec (r x y)
    (defun new-cdr (q n) (- (car n) (* q (cdr n))))
    (defun shift (q n) (cons (cdr n) (new-cdr q n)))
    (if (= (cdr r) 0)
	(list r x y)
	(let ((q (floor (/ (car r) (cdr r)))))
	  (extended-euclid-rec (shift q r) (shift q x) (shift q y)))))
  
  (let ((r a) (rp b)
	(x 1) (xp 0)
	(y 0) (yp 1))
    (extended-euclid-rec (cons r rp) (cons x xp) (cons y yp))))
    
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

(defun factorial-wheel (n)
  "Returns the prime factors composing n"
  
  (defun fib (n)
    (let ((phi (/ (+ 1 (sqrt 5)) 2)))
      (* (/ 1 (sqrt 5))
	 (- (expt (/ (+ 1 (sqrt 5)) 2) n)
	    (expt (/ (- 1 (sqrt 5)) 2) n)))))
  
  (defun is-prime (n)
    (if (= n 1)
	nil
	(or
					; fermat's little theorem
	 (= (modular-exponent 2 (- n 1) n) 1)
					; fibonacci test
	 (= (mod (fib (+ n 1)) n) 0))))
  
  (defun divides (d n)
    (= (mod n d) 0))
  
  (defun factorial-wheel-rec (n k inc)
    (if (> (expt k 2) n)
	(cons n nil)
	(if (divides k n)
	    (cons k (factorial-wheel-rec (/ n k) k inc))
	    (factorial-wheel-rec n (+ k (car inc)) (cdr inc)))))
  
  (let ((inc (list 1 2 2 4 2 4 2 4 6 2 6)))
    (setf (cdr (last inc)) (cdddr inc))
    (if (equal (is-prime n) t)
	'()
	(factorial-wheel-rec n 2 inc))))
