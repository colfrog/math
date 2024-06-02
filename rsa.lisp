(in-package :clf-math)

(defun get-prime-pair (n)
    (let ((pair (factorial-wheel n)))
      (if (> (length pair) 2)
	  nil
	  (cons (car pair) (cadr pair)))))

(defun cypher (M n e)
  (modular-exponent M e n))

(defun get-d (e prime-pair)
  (modular-inverse
   e
   (* (- (car prime-pair) 1)
      (- (cdr prime-pair) 1))))

(defun crack (C n e)
  (let* ((prime-pair (get-prime-pair n))
	 (d (get-d e prime-pair)))
    (modular-exponent C d n)))

(defun decypher (C n e p q)
  (let ((d (get-d e (cons p q))))
    (modular-exponent C d n)))
