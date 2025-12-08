(defun euclidean-distance (coord1 coord2)
  (destructuring-bind ((x1 y1 z1) (x2 y2 z2))
      (list coord1 coord2)
    (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2) (expt (- z1 z2) 2)))))

(defun parse-input ()
  (iter
    (for l in-file "day08-input.txt" using #'read-line)
    (collect (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" l)))))

(defun sort-by-distance (input)
  (mapcar #'rest
   (sort (iter outer
	   (for boxes on input)
	   (iter
	     (for box1 = (first boxes))
	     (for box2 in (rest boxes))
	     (for distance = (euclidean-distance box1 box2))
	     (in outer (collect (list distance box1 box2)))))
	 #'< :key #'car)))

(defun part1-final-calc (circuits total-boxes)
  (reduce #'*
	  (subseq (sort (concatenate 'vector
				     circuits
				     (make-array (- total-boxes (reduce #'+ circuits))
						 :initial-element 1))
			#'>)
		  0 3)))

(defun solve ()
  (let* ((boxes (make-hash-table :test 'equal))
	 (input (parse-input))
	 (circuits (make-array 1 :adjustable t :fill-pointer 0)))
    (iter
      (with part1)
      (for part1-counter from 0)
      (for pair in (sort-by-distance input))
      (for box1 = (gethash (first pair) boxes))
      (for box2 = (gethash (second pair) boxes))
      (for last-connected = (if (and box1 box2 (= box1 box2))
				last-connected
				pair))
      (generate new-circuit from 0)
      
      (cond
	;; connect box to pre-existent circuit
	((and box1 (not box2)) (progn (setf (gethash (second pair) boxes) box1)
				      (incf (aref circuits box1))))
	((and box2 (not box1)) (progn (setf (gethash (first pair) boxes) box2)
				      (incf (aref circuits box2))))
	;; join two already formed circuits
	((and box1 box2 (not (equal box1 box2))) (iter (for (box circuit) in-hashtable boxes)
						   (when (= circuit box2)
						     (setf (gethash box boxes) box1)
						     (incf (aref circuits box1)))
						   (finally (setf (aref circuits box2) 0))))
	;; make a new circuit from unconnected boxes
	((not (or box1 box2)) (let ((new (next new-circuit)))
				(setf (gethash (first pair) boxes) new
				      (gethash (second pair) boxes) new)
				(vector-push-extend 2 circuits))))

      (when (= part1-counter 999)
	(setf part1 (part1-final-calc circuits (length input))))
      (finally (return (values part1
			       (* (first (first last-connected))
				  (first (second last-connected)))))))))
