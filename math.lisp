(defun mod-expt (x n m)
  (defun z () (mod-expt (mod (* x x) m) (ash n (- 1)) m))
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
      (let ((q (round (/ (car r) (cdr r)))))
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

