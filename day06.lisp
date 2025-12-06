;;; Part 1

(let* ((worksheet (make-array 5000 :initial-contents (uiop:read-file-forms "day06-input.txt")))
       (ops (make-array 1000 :displaced-to worksheet :displaced-index-offset 4000)))
  (iter
    (for op :in-vector ops)
    (for i :index-of-vector ops)
    (for col := (iter
		  (for x from i to (+ i 3000) by 1000)
		  (if (eql op '+)
		      (reducing (aref worksheet x) by #'+)
		      (reducing (aref worksheet x) by #'*))))
    (sum col)))

;;; Part 2

(defun parse-input (&optional (file "day06-input.txt"))
  (let ((input (mapcar (lambda (line)
			 (iter (for ch in-string line) (collect ch)))
		       (uiop:read-file-lines file))))
    (make-array (list (length input) (length (first input)))
		:initial-contents input)))

(defun column (arr col)
  (make-array (array-dimension arr 0)
	      :initial-contents (iter
				  (for i from 0 below (array-dimension arr 0))
				  (collect (aref arr i col)))))

(let ((worksheet (parse-input)))
  (iter
    (for col from (1- (array-dimension worksheet 1)) downto 0)
    (for n = (parse-integer (coerce (column worksheet col)
				    'string)
			    :junk-allowed t))
    (when n (collect n into numbers))
    (for op = (case (aref worksheet (1- (array-dimension worksheet 0)) col)
		(#\+ #'+)
		(#\* #'*)))
    (when op
      (collect (reduce op numbers) into total)
      (setf numbers '()))
    
    (finally (return (reduce #'+ total)))))
