(load "3d-vector.lisp")

(defclass quaternion ()
  ((real :initarg :real :initform 0)
   (imag :initarg :imag :initform
      (make-instance '3d-vector))))

(defmethod show ((q quaternion))
  (with-slots (real imag) q
    (format nil "(~a, ~a)" real (show imag))))

(defmethod mul ((q quaternion) (p quaternion))
  (with-slots ((a real) (vq imag)) q
    (with-slots ((b real) (vp imag)) p
      (make-instance
       'quaternion
       :real (- (* a b) (dot vq vp))
       :imag (+ (mul vq a) (mul vp a) (cross vq vp))))))

(defmethod add ((q quaternion) (p quaternion))
  (with-slots ((a real) (vq imag)) q
    (with-slots ((b real) (vp imag)) p
      (make-instance
       'quaternion
       :real (+ a b)
       :imag (add vq vp)))))

(defmethod add ((q quaternion) (n number))
  (with-slots (real imag) q
    (make-instance
     'quaternion
     :real (+ real n)
     :imag imag)))

(defmethod add ((q quaternion) (v 3d-vector))
  (with-slots (real imag) q
    (make-instance
     'quaternion
     :real real
     :imag (add imag v))))

(defmethod sub ((q quaternion) (p quaternion))
  (with-slots ((a real) (vq imag)) q
    (with-slots ((b real) (vp imag)) p
      (make-instance
       'quaternion
       :real (- a b)
       :imag (sub vq vp)))))

(defmethod sub ((q quaternion) (n number))
  (with-slots (real imag) q
    (make-instance
     'quaternion
     :real (- real n)
     :imag imag))))

(defmethod sub ((q quaternion) (v 3d-vector))
  (with-slots (real imag) q
    (make-instance
     'quaternion
     :real real
     :imag (sub imag v))))
