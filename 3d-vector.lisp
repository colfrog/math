(defclass 3d-vector ()
  ((x :initarg :x :initform 0)
   (y :initarg :y :initform 0)
   (z :initarg :z :initform 0)))

; vector <-> number operation
(defmethod vnop ((f function) (u 3d-vector) (n number))
  (with-slots ((u1 x) (u2 y) (u3 z)) u
    (make-instance
     '3d-vector
     :x (funcall f u1 n)
     :y (funcall f u2 n)
     :z (funcall f u3 n))))

; vector <-> vector operation
(defmethod vvop ((f function) (u 3d-vector) (v 3d-vector))
  (with-slots ((u1 x) (u2 y) (u3 z)) u
    (with-slots ((v1 x) (v2 y) (v3 z)) v
      (make-instance
       '3d-vector
       :x (funcall f u1 v1)
       :y (funcall f u2 v2)
       :z (funcall f u3 v3)))))

(defmethod mul ((u 3d-vector) (n number))
  (vnop #'* u n))

(defmethod div ((u 3d-vector) (n number))
  (vnop #'/ u n))

(defmethod add ((u 3d-vector) (v 3d-vector))
  (vvop #'+ u v))

(defmethod sub ((u 3d-vector) (v 3d-vector))
  (vvop #'- u v))

(defmethod show ((u 3d-vector))
  (with-slots (x y z) u
    (format nil "[~a ~a ~a]" x y z)))

(defmethod norm ((u 3d-vector))
  (with-slots (x y z) u
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defmethod dot ((u 3d-vector) (v 3d-vector))
  (with-slots ((u1 x) (u2 y) (u3 z)) u
    (with-slots ((v1 x) (v2 y) (v3 z)) v
      (+ (* u1 v1) (* u2 v2) (* u3 v3)))))

(defmethod cross ((u 3d-vector) (v 3d-vector))
  (with-slots ((u1 x) (u2 y) (u3 z)) u
    (with-slots ((v1 x) (v2 y) (v3 z)) v
      (let
	  ((w1 (- (* u2 v3) (* u3 v2)))
	   (w2 (- (* u3 v1) (* u1 v3)))
	   (w3 (- (* u1 v2) (* u2 v1))))
	(make-instance '3d-vector :x w1 :y w2 :z w3)))))

(defmethod projection ((u 3d-vector) (v 3d-vector))
  (mul v (/ (dot u v) (expt (norm v) 2))))

(defmethod make-unit ((u 3d-vector))
  (with-slots (x y z) u
    (let ((n (norm u)))
      (make-instance
       '3d-vector
       :x (/ x n)
       :y (/ y n)
       :z (/ z n)))))
