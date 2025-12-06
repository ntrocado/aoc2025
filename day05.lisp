;;; Part 1

(defun is-fresh-p (ranges ingredient)
  (iter
    (for (low high) in ranges)
    (thereis (<= low ingredient high))))

(defun parse-input ()
  (iter
    (with blank-line-p)
    (for l in-file "day05-input.txt" using #'read-line)
    (if (string= l "")
	(setf blank-line-p t)
	(if blank-line-p
	    (collect (parse-integer l) into items)
	    (collect (mapcar #'parse-integer
			     (uiop:split-string l :separator "#\-"))
	      into ranges)))
    (finally (return (values items ranges)))))

(multiple-value-bind (items ranges)
    (parse-input)
  (count ranges items :test #'is-fresh-p))

;;; Part 2

(defun combine (a b)
  (cond ((not b) (list a))
	((or (> (first b) (second a)) (< (second b) (first a)))
	 (list a b))
	(t (let ((s (append a b)))
	     (list (list (apply #'min s)
			 (apply #'max s)))))))

(defun merge-ranges (ranges)
  (let ((sorted-ranges (sort (copy-seq ranges) #'< :key #'car)))
    (iter
      (for range in sorted-ranges)
      (accumulate range by (lambda (range acc)
			     (nconc (combine range (first acc))
				    (rest acc)))))))

(multiple-value-bind (items ranges)
    (parse-input)
  (declare (ignore items))
  (reduce #'+ (merge-ranges ranges) :key (lambda (x) (1+ (- (second x) (first x))))))
