;;; Part 1

(defun rotate (current instruction)
  (let ((op (if (char= (aref instruction 0) #\L) #'- #'+)))
    (mod (funcall op current (parse-integer (subseq instruction 1)))
	 100)))

(with-open-file (in "day1-input.txt")
  (loop :for line := (read-line in nil)
	:while line
	:for current := (rotate 50 line) :then (rotate current line)
	:count (zerop current)))

;;; Part 2

(defun rotate (current instruction)
  (let* ((op (if (char= (aref instruction 0) #\L) #'- #'+))
	 (n (funcall op current (parse-integer (subseq instruction 1))))
	 (final-pos (mod n 100))
	 (zeros (cond
		  ((eql op #'+) (floor n 100))
		  ((and (zerop current)       (not (zerop final-pos)) (1- (abs (floor n 100)))))
		  ((and (not (zerop current)) (zerop final-pos))      (1+ (abs (floor n 100))))
		  (t (abs (floor n 100))))))
    (values final-pos zeros)))

(with-open-file (in "day1-input.txt")
  (loop :for line := (read-line in nil)
	:while line
	:for (current zeros) := (multiple-value-list (rotate 50 line))
	  :then (multiple-value-list (rotate current line))
	:sum zeros))
