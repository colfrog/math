(defun modular-exponent (x n m)
  (defun z () (modular-exponent (mod (* x x) m) (ash n (- 1)) m))
  (cond
    ((= n 0) 1)
    ((= (logand n 1) 1)
     (mod (* x (z)) m))
    (t (z))))

(defun extended-euclid (a b)
  (let ((r a) (rp b)
	(x 1) (xp 0)
	(y 0) (yp 1))
    (extended-euclid-rec (cons r rp) (cons x xp) (cons y yp))))

(defun extended-euclid-rec (r x y)
  (defun new-cdr (q n) (- (car n) (* q (cdr n))))
  (defun shift (q n) (cons (cdr n) (new-cdr q n)))
  (if (= (cdr r) 0)
      (list r x y)
      (let ((q (floor (/ (car r) (cdr r)))))
	(extended-euclid-rec (shift q r) (shift q x) (shift q y)))))
    
(defun modular-inverse (x mod)
   (cond
    ((or (= x 0) (<= mod 0)) 0)
    ((or (< x 2) (< mod 2)) 1)
    (t (let ((l (extended-euclid x mod)))
	 (cond
	   ((not (= (caar l) 1)) l)
	   ((< (caadr l) 0) (+ (caadr l) mod))
	   (t (caadr l)))))))

(defun fib (n)
  (do ((i n (1- i))
       (f1 0 f2)
       (f2 1 (+ f1 f2)))
      ((= i 0) f1)))

(defun is-prime (n)
  (and
   ; fermat's little theorem
   (= (modular-exponent 2 (- n 1) n) 1)
   ; fibonacci test
   (= (mod (fib (+ n 1)) n) 0)))

(defun divides (d n)
  (= (mod n d) 0))

(defun sum (l)
  (if (consp l)
      (+ (car l) (product (cdr l)))
      0))

(defun product (l)
  (if (consp l)
      (* (car l) (product (cdr l)))
      1))

(defun factorial-wheel (n)
  (let ((inc (list 1 2 2 4 2 4 2 4 6 2 6)))
    (setf (cdr (last inc)) (cdddr inc))
    (if (equal (is-prime n) t)
	'()
	(factorial-wheel-rec n 2 inc))))

(defun factorial-wheel-rec (n k inc)
  (print k)
  (print n)
  (if (> (expt k 2) n)
      (cons n nil)
      (if (divides k n)
	  (cons k (factorial-wheel-rec (/ n k) k inc))
	  (factorial-wheel-rec n (+ k (car inc)) (cdr inc)))))
