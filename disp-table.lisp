(in-package :clf-math)

(defparameter *disp-table-initial-size* 7)
(defclass disp-table ()
  ((data :initform (make-array *disp-table-initial-size* :initial-element nil))
   (keys :initform (make-array *disp-table-initial-size* :initial-element nil))
   (flags :initform (make-array *disp-table-initial-size* :initial-element nil))
   (n :initform 0)
   (size :initform *disp-table-initial-size*)
   (hash :initform
	 (lambda (key)
	   (let ((sum 1)
		 (c 37))
	     (dotimes (i (length key))
	       (setf sum (+ (* c sum) (char-int (aref key i)))))
	     sum)))))

(defparameter *disp-table-occupied* 1)
(defparameter *disp-table-deleted* -1)

(defmethod set-hash-function-disp-table ((tab disp-table) f)
  (setf (slot-value tab 'hash) f)
  (rehash-disp-table tab 1))

(defmethod rehash-disp-table ((tab disp-table) order)
  (with-slots (data keys flags size) tab
    (let* ((new-size (next-prime (* order size)))
	   (old-size size)
	   (old-flags flags)
	   (old-keys keys)
	   (old-data data))
      (setf size new-size)
      (setf data (make-array new-size :initial-element nil))
      (setf keys (make-array new-size :initial-element nil))
      (setf flags (make-array new-size :initial-element nil))
      (dotimes (i old-size)
	(when (eq (aref old-flags i) *disp-table-occupied*)
	  (set-disp-table tab (aref old-keys i) (aref old-data i)))))))

(defmethod index-dist-table ((tab disp-table) key)
  (with-slots (hash keys flags size) tab
    (do ((offset 1 (+ offset 2))
	 (current (mod (funcall hash key) size) (mod (+ current offset) size)))
	((or (null (aref flags current)) (equal (aref keys current) key))
	 current))))

(defmethod set-disp-table ((tab disp-table) key value)
  (with-slots (data keys flags size n) tab
    (let ((index (index-dist-table tab key)))
      (when (not (eq (aref flags index) *disp-table-occupied*))
	(setf n (1+ n)))
      (setf (aref flags index) *disp-table-occupied*)
      (setf (aref data index) value)
      (setf (aref keys index) key)
      (when (> n (/ size 2))
	(rehash-disp-table tab 2)))))

(defmethod remove-disp-table ((tab disp-table) key)
  (with-slots (data flags n) tab
    (let ((index (index-dist-table tab key)))
      (when (eq (aref flags index) *disp-table-occupied*)
	(setf (aref flags index) *disp-table-deleted*)
	(setf (aref data index) nil)))))

(defmethod get-disp-table ((tab disp-table) key)
  (with-slots (data flags) tab
    (let ((index (index-dist-table tab key)))
      (when (eq (aref flags index) *disp-table-occupied*)
	(aref data index)))))
