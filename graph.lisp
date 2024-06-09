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
		    (visited array) &key start end clock)
  (with-slots (data) g
    (setf (aref visited node) t)
    (when (and (not (null start)) (not (null clock)))
      (setf (aref start node) clock))
    (dolist (adj-node (aref data node))
      (when (not (aref visited adj-node))
	(setf clock (explore g adj-node stack visited
			     :start start :end end :clock (when (not (null clock))
							    (1+ clock))))))
    (when (not (null clock))
      (setf clock (1+ clock)))
    (when (and (not (null end)) (not (null clock)))
      (setf (aref end node) clock))
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
	  (setf clock (1+ (explore g i stack visited
				   :start start :end end :clock clock)))))
      (list stack start end))))

(defmethod kosaraju ((g graph))
  (with-slots (data) g
    (let* ((size (length data))
	   (stacks nil)
	   (visited (make-array size :initial-element nil))
	   (invg (inverted-graph g))
	   (invg-stack (car (in-depth invg))))
      (do ((node (pop-stack invg-stack) (pop-stack invg-stack))
	   (stack (make-instance 'stack) (make-instance 'stack)))
	  ((null (slot-value invg-stack 'data)) nil)
	(when (not (aref visited node))
	  (explore g node stack visited)
	  (push stack stacks)))
      stacks)))

(defmethod print-object ((g graph) stream)
  (print (slot-value g 'data) stream))
