(load "probabilite-discrete.lisp")

(defun approximation-integrale (f a b dx)
  (do* ((x1 a (+ x1 dx))
	(x2 (+ a dx) (+ x2 dx))
	(y1 (funcall f x1)
	    (funcall f x1))
	(y2 (funcall f x2)
	    (funcall f x2))
	(h (/ (+ y1 y2) 2) (/ (+ y1 y2) 2))
	(somme (* h dx) (+ somme (* h dx))))
       ((>= x2 b) somme)))

(defun approximation-integrale-infinie (f a dx)
  (let ((x-max
	   (do* ((x a (+ x dx))
		 (y (funcall f x) (funcall f x)))
		((and (<= y 0.0001) (/= (round x) 0))
		 x))))
    (approximation-integrale f a x-max dx)))

(defun fonction-gamma (alpha)
  (if (= alpha (round alpha))
      (factorielle (- alpha 1))
      (approximation-integrale-infinie
       (lambda (x) (* (expt x (- alpha 1)) (exp (- x))))
       0 0.1)))

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

  (let* ((moyenne (/ r taux))
	 (variance (/ r (expt taux 2)))
	 (ecart-type (sqrt variance))
	 (densité
	   (lambda (x)
	     (*
	      (/ taux (fonction-gamma r))
	      (expt (* taux x) (- r 1))
	      (exp (* (- taux) x))))))

    (list
     ; Densité
     (lambda (x)
       (if (<= x 0)
	   0
	   (funcall densité x)))
     ; Répartition
     (lambda (x)
       (if (<= x 0)
	   0
	   (if (= (round r) r)
	       (let ((somme-intégrée
		       (lambda (k)
			 (/ (*
			     (exp (* (- taux) x))
			     (expt (* taux x) k))
			    (factorielle k)))))
		 (- 1
		    (do* ((k 0 (1+ k))
			  (sum (funcall somme-intégrée k)
			       (+ sum (funcall somme-intégrée k))))
			 ((>= k (- r 1)) sum))))
	       (approximation-integrale densité 0 x 0.001))))
     moyenne
     variance
     ecart-type)))

(defun Weibull (gamma beta delta)
  ;; Weibull est une exponentielle à trois paramètres. Elle demande
  ;; le paramètre de forme beta qui controle la direction de la fonction,
  ;; le paramètre de dispersion delta est proportionnel à la variance,
  ;; le paramètre gamma détermine la position de la courbe
 
  (let* ((moyenne (+ gamma
		     (* delta
			(fonction-gamma (+ 1 (/ 1 beta))))))
	 (variance (*
		    (expt delta 2)
		    (-
		     (fonction-gamma (+ 1 (/ 2 beta)))
		     (expt (fonction-gamma (+ 1 (/ 1 beta))) 2))))
	 (ecart-type (sqrt variance)))
 
    (list
     ; Densité
     (lambda (x)
       (if (< x gamma)
	   0
	   (*
	    (/ beta delta)
	    (expt (/ (- x gamma) delta) (- beta 1))
	    (exp (- (expt (/ (- x gamma) delta) beta))))))
     ; Répartition
     (lambda (x)
       (if (< x gamma)
	   0
	   (- 1 (exp (- (expt (/ (- x gamma) delta) beta))))))
     moyenne
     variance
     ecart-type)))

(defun Normale (mu sigma2)
  ;; La loi normale prend sa moyenne et son écart type en argument,
  ;; elle modélise une courbe qui suit ces propriétés et qui est définie
  ;; sur tout le domaine de x, se rapprochant à zéro en s'éloignant de
  ;; la moyenne.

  (let* ((moyenne mu)
	 (variance sigma2)
	 (ecart-type (sqrt sigma2))
	 (densite (lambda (x)
		    (*
		     (/ 1 (* ecart-type (sqrt (* 2 PI))))
		     (exp (* (- (/ 1 2))
			     (expt (/ (- x mu) ecart-type) 2)))))))

	 (list
	   ; Densité
	   densite
	   ; Répartition
	   (lambda (x)
	     (let ((limite (* 4 ecart-type)))
	       (if (< x (- mu limite))
		   0
		   (approximation-integrale
		    densite
		    (- mu limite)
		    (min (+ mu limite) x)
		    (/ ecart-type 1000)))))
	   moyenne
	   variance
	   ecart-type)))

(defun repartition-continue (loi x)
  (funcall (cadr loi) x))

(defun densite (loi x)
  (funcall (car loi) x))
