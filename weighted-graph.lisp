(in-package :clf-math)

(defclass weighted-graph ()
  ((data :initform nil :initarg :data)))

(defmethod add-node-w ((g weighted-graph) &key dest-nodes src-nodes)
  (with-slots (data) g
    (let* ((size (1+ (car (array-dimensions data))))
	   (new-matrix (make-array (list size size) :initial-element nil)))
      (dotimes (i (1- size))
	(dotimes (j (1- size))
	  (setf (aref new-matrix i j) (aref data i j))))
      (setf (aref new-matrix (1- size) (1- size)) 0)
      (dolist (node dest-nodes)
	(setf (aref new-matrix (1- size) (car node)) (cdr node)))
      (dolist (node src-nodes)
	(setf (aref new-matrix (car node) (1- size)) (cdr node))))))

(defmethod add-arc-w ((g weighted-graph) (src-node number) (dest-node number) (weight number))
  (with-slots (data) g
    (setf (aref data src-node dest-node) weight)))

(defun index-of-min (arr)
  (let ((minval nil)
	(min-index nil))
    (dotimes (i (length arr))
      (let ((val (aref arr i)))
	(when (not (null val))
	  (when (or (null minval) (< val minval))
	    (setf minval val)
	    (setf min-index i)))))
    min-index))

(defun and-array (arr)
  (let ((res t))
    (dotimes (i (length arr))
      (setf res (and res (aref arr i))))
    res))

(defun mask-array (mask arr)
  (let ((new-array (make-array (length arr) :initial-element nil)))
    (dotimes (i (length arr))
      (when (aref mask i)
	(setf (aref new-array i) (aref arr i))))
    new-array))

(defmethod dijkstra ((g weighted-graph) (start-node number))
  (with-slots (data) g
    (let* ((size (car (array-dimensions data)))
	   (distance (make-array size :initial-element nil))
	   (predecessor (make-array size :initial-element nil))
	   (done (make-array size :initial-element nil))
	   (not-done (make-array size :initial-element t)))
      (setf (aref distance start-node) 0)
      (do ((node start-node (index-of-min (mask-array not-done distance))))
	  ((and-array done) nil)
	(setf (aref done node) t)
	(setf (aref not-done node) nil)
	(dotimes (u size)
	  (let ((weight (aref data node u)))
	    (when (and (aref not-done u) (not (null weight)))
	      (let ((d (+ (aref distance node) weight)))
		(setf (aref distance u) d)
		(setf (aref predecessor u) node))))))
      (cons distance predecessor))))

(defmethod bellman-ford ((g weighted-graph) (start-node number))
  (with-slots (data) g
    (let* ((size (car (array-dimensions data)))
	   (distance (make-array size :initial-element nil))
	   (predecessor (make-array size :initial-element nil))
	   (changed t))
      (setf (aref distance start-node) 0)
      (do ((count 0 (1+ count)))
	  ((or (null changed) (= count (- size 1))) nil)
	(setf changed nil)
	(dotimes (u size)
	  (dotimes (v size)
	    (let ((weight (aref data u v))
		  (du (aref distance u)))
	      (when (and (/= u v) (not (null weight)) (not (null du)))
		(let ((dv-calc (+ du weight))
		      (dv-ref (aref distance v)))
		  (when (or (null dv-ref) (< dv-calc dv-ref))
		    (setf changed t)
		    (setf (aref distance v) dv-calc)
		    (setf (aref predecessor v) u))))))))
      (if changed nil
	  (cons distance predecessor)))))

(defmethod topological-sort ((g weighted-graph))
  (with-slots (data) g
    (let* ((size (car (array-dimensions data)))
	   (input-degree (make-array size :initial-element 0))
	   (queue (make-instance 'queue))
	   (result (make-instance 'queue)))
      (dotimes (i size)
	(dotimes (j size)
	  (let ((weight (aref data j i)))
	    (when (not (or (= i j) (null weight)))
	      (setf (aref input-degree i) (1+ (aref input-degree i)))))))
      (dotimes (i size)
	(when (= (aref input-degree i) 0)
	  (push-queue queue i)))
      (do () ((null (slot-value queue 'data)) nil)
	(let ((node (pop-queue queue)))
	  (push-queue result node)
	  (dotimes (u size)
	    (when (/= node u)
	      (when (not (null (aref data node u)))
		(setf (aref input-degree u) (1- (aref input-degree u)))
		(when (= (aref input-degree u) 0)
		  (push-queue queue u)))))))
      (let ((node-order (slot-value result 'data)))
	(if (= (length node-order) size)
	    node-order nil)))))

(defmethod bellman-ford-acyclic ((g weighted-graph) (start-node number))
  (with-slots (data) g
    (let* ((size (car (array-dimensions data)))
	   (order (topological-sort g))
	   (distance (make-array size :initial-element nil))
	   (predecessor (make-array size :initial-element nil)))
      (setf (aref distance start-node) 0)
      (dolist (u order)
	(dotimes (v size)
	  (let ((weight (aref data u v))
		(du (aref distance u)))
	    (when (and (/= u v) (not (null weight)) (not (null du)))
	      (let ((dv-calc (+ du weight))
		    (dv-ref (aref distance v)))
		(when (or (null dv-ref) (< dv-calc dv-ref))
		  (setf (aref distance v) dv-calc)
		  (setf (aref predecessor v) u)))))))
      (when (not (null order))
	(cons distance predecessor)))))

(defmethod floyd ((g weighted-graph))
  (with-slots (data) g
    (let ((size (car (array-dimensions data)))
	  (distance (make-array (array-dimensions data)))
	  (predecessor (make-array (array-dimensions data) :initial-element nil)))
      (dotimes (i size)
	(dotimes (j size)
	  (setf (aref distance i j) (aref data i j))
	  (setf (aref predecessor i j)
		(when (and (/= i j) (not (null (aref data i j)))) i))))
      (dotimes (k size)
	(dotimes (i size)
	  (dotimes (j size)
	    (let ((d1 (aref distance i j))
		  (d2 (aref distance i k))
		  (d3 (aref distance k j)))
	      (setf (aref distance i j)
		    (if (and (not (null d2)) (not (null d3))
			     (or (null d1) (> d1 (+ d2 d3))))
			(progn
			  (setf (aref predecessor i j) (aref predecessor k j))
			  (+ d2 d3))
			d1))))))
      (cons distance predecessor))))

(defmethod print-object ((g weighted-graph) stream)
  (print (slot-value g 'data) stream))
