(in-package :clf-math)

(defclass stack ()
  ((data :initarg :data :initform nil)))

(defmethod push-stack ((stack stack) elem)
  (with-slots (data) stack
    (setf data (cons elem data))))

(defmethod pop-stack ((stack stack))
  (with-slots (data) stack
    (let ((elem (car data)))
      (setf data (cdr data))
      elem)))

(defmethod print-object ((stack stack) stream)
  (print (slot-value stack 'data) stream))
