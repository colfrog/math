(load "probabilite-discrete.lisp")

(defun approximation-integrale (f a b dx)
  (do* ((x1 a (+ x1 dx))
	(x2 (+ a dx) (+ x2 dx))
	(y1 (funcall f x1)
	    (funcall f x1))
	(y2 (funcall f x2)
	    (funcall f x2))
	(h (/ (+ y1 y2) 2) (/ (+ y1 y2) 2))
	(sum (* h dx) (+ sum (* h dx))))
       ((>= x2 b) sum)))

(defun approximation-integrale-infinie (f a dx)
  (let ((x-max
	   (do* ((x a (+ x dx))
		 (y (funcall f x) (funcall f x)))
		((<= y 0.0001) x))))
    (approximation-integrale f a x-max dx)))

(defun Uniforme (alpha beta)
  ;; La loi uniforme a une fonction de densité constante
  ;; dans un intervalle [alpha, beta]

  (let* ((moyenne (/ (+ beta alpha) 2))
	 (variance (/ (expt (- beta alpha) 2) 12))
	 (ecart-type (sqrt variance)))

    (list
     ; Densité
     (lambda (x)
       (if (or (< x alpha) (> x beta))
	   0
	   (/ 1 (- beta alpha))))
     ; Répartition
     (lambda (x)
       (cond
	 ((< x alpha) 0)
	 ((> x beta) 1)
	 (t (/ (- x alpha) (- beta alpha)))))
     moyenne
     variance
     ecart-type)))

(defun Exponentielle (taux)
  ;; La loi exponentielle décrit le temps qui s'écoule entre deux événements

  (let* ((moyenne (/ 1 taux))
	 (variance (/ 1 (expt taux 2)))
	 (ecart-type moyenne))

    (list
     ; Densité
     (lambda (x)
       (if (< x 0)
	   0
	   (* taux (exp (* (- taux) x)))))
     ; Répartition
     (lambda (x)
       (if (< x 0)
	   0
	   (- 1 (exp (* (- taux) x)))))
     moyenne
     variance
     ecart-type)))

(defun Gamma (r taux)
  ;; La loi Gamma donne le temps pour r fonctions exponentielles d'arriver
  ;; selon le taux donné. Cette fonction est écrite pour une valeur entière
  ;; de r.

  (let* ((fonction-gamma
	   (lambda (alpha) (factorielle (- alpha 1))))
	 (moyenne (/ 1 taux))
	 (variance (/ 1 (expt taux 2)))
	 (ecart-type moyenne))

    (list
     ; Densité
     (lambda (x)
       (if (<= x 0)
	   0
	   (*
	    (/ taux (funcall fonction-gamma r))
	    (expt (* taux x) (- r 1))
	    (exp (* (- taux) x)))))
     ; Répartition
     (lambda (x)
       (if (<= x 0)
	   0
	   (let ((repartition (lambda (k)
				(/ (*
				    (exp (* (- taux) x))
				    (expt (* taux x) k))
				   (factorielle k)))))
	   (- 1
	      (do* ((k 0 (1+ k))
		    (sum (funcall repartition k)
			 (+ sum (funcall repartition k))))
		   ((>= k (- r 1)) sum))))))
     moyenne
     variance
     ecart-type)))

(defun repartition-continue (loi x)
  (funcall (cadr loi) x))

(defun densite (loi x)
  (funcall (car loi) x))
