;;; Part 1

(defun invalid-p (id)
  (let ((len (length id)))
    (when (evenp len)
      (string= (subseq id 0 (/ len 2))
	       (subseq id (/ len 2))))))

(defun in-range (from to)
  (loop :for n :from from :upto to
	:when (invalid-p (format nil "~a" n))
	  :sum n))

(let ((sum 0))
  (ppcre:do-register-groups ((#'parse-integer to)
			     (#'parse-integer from))
      ("(\\d+)-(\\d+)" (uiop:read-file-string "day02-input.txt") sum :sharedp t)
    (incf sum (in-range to from))))

;;; Part 2

(let ((memo (make-hash-table)))
  (defun divisors (n)
    (or (gethash n memo)
	(setf (gethash n memo)
	      (remove n
		      (remove-duplicates
		       (loop :for i :from 1 :upto (sqrt n)
			     :when (zerop (mod n i))
			       :collect i :and :collect (/ n i))))))))

(defun invalid-p (id)
  (loop :for div :in (divisors (length id))
	:for regex := (format nil "\\d{~d}" div)
	:for matches := (mapcar #'parse-integer
				(ppcre:all-matches-as-strings regex id :sharedp t))
	:thereis (every #'eql matches (rest matches))))
