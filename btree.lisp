(in-package :clf-math)

(defun make-tree (value &optional (node1 nil) (node2 nil))
  (cons
   value
   (cons node1 node2)))

(defun insert-tree (tree value)
  (if tree
      (if (<= value (car tree))
	  (make-tree (car tree)
		     (insert-tree (cadr tree) value)
		     (cddr tree))
	  (make-tree (car tree)
		     (cadr tree)
		     (insert-tree (cddr tree) value)))
      (make-tree value)))

(defun build-tree (l &optional (tree nil))
  (if (car l)
      (build-tree (cdr l) (insert-tree tree (car l)))
      tree))
