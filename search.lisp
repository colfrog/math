(in-package :clf-math)

(defun random-array (size max)
  (let ((a (make-array size)))
    (dotimes (i size)
      (setf (aref a i) (random max)))
    a))

(defun iterative-search (a val)
  (let ((ret -1))
    (dotimes (i (length a))
      (when (= val (aref a i))
	(setf ret i)
	(return)))
    ret))

(defun binary-search (a val &key start end)
  ;; a must be sorted
  (when (null start)
    (setf start 0))
  (when (null end)
    (setf end (1- (length a))))
  (if (= start end)
      (if (= (aref a start) val)
	  start
	  nil)
      (let* ((index (floor (/ (+ end start) 2)))
	     (index-val (aref a index)))
	(cond
	  ((= index-val val) index)
	  ((> val index-val)
	   (binary-search a val :start (1+ index) :end end))
	  ((< val index-val)
	   (binary-search a val :start start :end (1- index)))))))

(defun selection (a k &key start end)
  (when (null start)
    (setf start 0))
  (when (null end)
    (setf end (1- (length a))))
  ;; a doesn't have to be sorted
  (let* ((s (partition a start end))
	 (nth (1+ (- s start))))
    (cond
      ((= nth k) (aref a s))
      ((< nth k) (selection a (+ (- k s) (1- start)) :start (1+ s) :end end))
      ((> nth k) (selection a k :start start :end (1- s))))))
