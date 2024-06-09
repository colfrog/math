(in-package :clf-math)

(defclass graph ()
  ((data :initform nil :initarg :data)))

(defmethod add-node ((g graph) (arcs list))
  (with-slots (data) g
    (let* ((new-length (1+ (length data)))
	   (a (make-array new-length)))
      (dotimes (i (length data))
	(setf (aref a i) (aref data i)))
      (setf (aref a new-length) arcs)
      (setf data a))))

(defmethod add-arc ((g graph) (index number) (target number))
  (with-slots (data) g
    (push target (aref data index))))

(defmethod inverted-graph ((g graph))
  (let ((invg (make-instance 'graph)))
    (with-slots ((data-g data)) g
      (with-slots ((data-invg data)) invg
	(setf data-invg (make-array (length data-g) :initial-element nil))
	(dotimes (i (length data-g))
	  (dolist (node (aref data-g i))
	    (push i (aref data-invg node))))))
    invg))

(defmethod explore ((g graph) (node number) (stack stack)
		    (visited array) (start array) (end array) (clock number))
  (with-slots (data) g
    (setf (aref visited node) t)
    (setf (aref start node) clock)
    (dolist (adj-node (aref data node))
      (when (not (aref visited adj-node))
	(setf clock (explore g adj-node stack visited start end (1+ clock)))))
    (setf clock (1+ clock))
    (setf (aref end node) clock)
    (push-stack stack node)
    clock))

(defmethod in-depth ((g graph))
  (with-slots (data) g
    (let* ((size (length data))
	   (clock 1)
	   (stack (make-instance 'stack))
	   (visited (make-array size :initial-element nil))
	   (start (make-array size :element-type 'number :initial-element -1))
	   (end (make-array size :element-type 'number :initial-element -1)))
      (dotimes (i size)
	(when (not (aref visited i))
	  (setf clock (1+ (explore g i stack visited start end clock)))))
      (list stack start end))))

(defmethod print-object ((g graph) stream)
  (print (slot-value g 'data) stream))
