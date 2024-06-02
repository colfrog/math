(in-package :clf-math)

(defclass queue ()
  ((data :initarg :data :initform nil)
   (last :initform nil)))

(defmethod initialize-instance :after ((q queue) &key)
  (with-slots (data last) q
    (do ((l data (cdr l)))
	((null l) nil)
      (when (null (cdr l))
	(setf last l)))))

(defmethod push-queue ((q queue) elem)
  (with-slots (data last) q
    (if (null data)
	(progn
	  (setf data (list elem))
	  (setf last data))
	(progn
	  (setf (cdr last) (list elem))
	  (setf last (cdr last))))
    data))

(defmethod pop-queue ((q queue))
  (with-slots (data last) q
    (let ((elem (car data)))
      (setf data (cdr data))
      (when (null data)
	(setf last nil))
      elem)))
