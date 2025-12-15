(iter
  (for line in-file "day12-input.txt" using #'read-line)
  (for i from 1)
  (when (>= i 31)
    (destructuring-bind (x y &rest shapes)
	(mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d{2}" line))
      (counting (> (reduce #'+ (mapcar (lambda (x) (* x 9)) shapes)) (* x y)) into r)))
  (finally (return (- (- i 30) r))))
