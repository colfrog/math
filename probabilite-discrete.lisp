(in-package :clf-math)

(defun factorielle (n)
  (cond
    ((<= n 1) 1)
    (t (* n (factorielle (- n 1))))))

(defun permutations (n x)
  (/ (factorielle n) (factorielle (- n x))))

(defun combinaisons (n x)
  (/ (permutations n x) (factorielle x)))

(defun Bernoulli (p)
  ;; Bernouilli représente un choix binaire
  ;; Chaque épreuve a une probabilité p de réussir
  ;; 1 pour réussite, 0 pour échec
  
  (let* ((moyenne p)
	 (variance (* p (- 1 p)))
	 (ecart-type (sqrt variance)))
    (list
     (lambda (x)
       (cond
	 ((= x 0) (- 1 p))
	 ((= x 1) p)
	 (t 0)))
     moyenne variance ecart-type)))

(defun Binomiale (n p)
  ;; La loi binomiale représente le nombre de succès obtenus
  ;; en réalisant n épreuves de Bernoulli(p) indépendantes
  
  (let* ((moyenne (* n p))
	 (variance (* moyenne (- 1 p)))
	 (ecart-type (sqrt variance)))
    (list
     (lambda (x)
       (if (< x 0)
	   0
	   (*
	    (combinaisons n x)
	    (expt p x)
	    (expt (- 1 p) (- n x)))))
     moyenne
     variance
     ecart-type)))

(defun Geometrique (p)
  ;; La loi géométrique représente le nombre d'épreuves nécessaires
  ;; pour obtenir un premier succès selon la probabilité p

  (let* ((moyenne (/ 1 p))
	 (variance (/ (- 1 p) (expt p 2)))
	 (ecart-type (sqrt variance)))
    (list
     (lambda (x)
       (if (<= x 0)
	   0
	   (* (expt (- 1 p) (- x 1)) p)))
     moyenne
     variance
     ecart-type)))

(defun Pascal (r p)
  ;; La loi de Pascal désigne le nombre d'épreuves nécessaires pour
  ;; obtenir le r-ième succès d'une épreuve de Bernoulli de probabilité p.

  (let* ((moyenne (/ r p))
	 (variance (/ (* r (- 1 p)) (expt p 2)))
	 (ecart-type (sqrt variance)))

    (list
     (lambda (x)
       (if (< x r)
	   0
	   (*
	    (combinaisons (- x 1) (- r 1))
	    (expt p r)
	    (expt (- 1 p) (- x r)))))
     moyenne
     variance
     ecart-type)))

(defun Poisson (c)
  ;; La loi de Poisson est un cas limite de la loi binomiale lorsque
  ;; n est grand et p est petit. Son paramètre c représente sa moyenne.
  ;; On observe c événements par unité de temps

  (let ((moyenne c)
	(variance c)
	(ecart-type (sqrt c)))

    (list
     (lambda (x)
       (if (< x 0)
	   0
	   (/
	    (* (expt c x) (exp (- c)))
	    (factorielle x))))
     moyenne
     variance
     ecart-type)))

(defun repartition-discrete (loi x-max &optional (depart 0) (pas 1))
  (do* ((x depart (+ x pas))
	(sum (funcall (car loi) x)
	     (+ sum (funcall (car loi) x))))
       ((>= x x-max) sum)))

(defun masse (loi x)
  (funcall (car loi) x))
