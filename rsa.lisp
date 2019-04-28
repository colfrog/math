(load "discrete.lisp")

(defun get-prime-pair (n)
    (let ((pair (factorial-wheel n)))
      (if (> (length pair) 2)
	  nil
	  (cons (car pair) (cadr pair)))))

(defun cypher (e n M)
  (modular-exponent M e n))

(defun decypher (e n C)
  (let* ((prime-pair (get-prime-pair n))
	 (d
	  (modular-inverse
	   e
	   (* (- (car prime-pair) 1)
	      (- (cdr prime-pair) 1)))))
    (modular-exponent C d n)))
