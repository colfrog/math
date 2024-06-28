(in-package :clf-math)

(defclass dyn-array ()
  ((data :initform nil)
   (size :initform 0 :initarg :size)))

(defmethod initialize-instance :after ((a dyn-array) &key)
  (with-slots (size) a
    (resize-dyn-array a size)))

(defmethod resize-dyn-array ((a dyn-array) (size number))
  (with-slots (data) a
    (let ((n 1))
      (do ()
	  ((>= n size) nil)
	(setf n (ash n 1)))
      (let ((new-array (make-array n :initial-element nil)))
	(dotimes (i (length data))
	  (setf (aref new-array i) (aref data i)))
	(setf data new-array)))))

(defmethod push-dyn-array ((a dyn-array) value)
  (with-slots (data size) a
    (if (>= (1+ size) (length data))
	(let ((new-data (resize-dyn-array a (1+ size))))
	  (setf (aref new-data size) value))
	(setf (aref data size) value))
    (setf size (1+ size))))

(defmethod set-dyn-array ((a dyn-array) (i number) value)
  (with-slots (data size) a
    (when (< i size)
      (setf (aref data i) value))))

(defmethod get-dyn-array ((a dyn-array) (i number))
  (with-slots (data size) a
    (when (< i size)
      (aref data i))))

(defmethod pop-dyn-array ((a dyn-array))
  (with-slots (data size) a
    (setf size (1- size))
    (aref data size)))
