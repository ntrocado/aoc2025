(defun parse-input ()
  (let* ((ht (make-hash-table :test 'equal)))
    (iter
      (for l in-file "day11-input.txt" using #'read-line)
      (for ss = (ppcre:all-matches-as-strings "[a-z]{3}" l))
      (for (device outputs) := (list (first ss) (rest ss)))
      (setf (gethash device ht) outputs))
    ht))

(let ((memo (make-hash-table :test 'equal)))
  (defun path-count (node dest ht)
    (or (gethash (list node dest ht) memo)
	(setf (gethash (list node dest ht) memo)
	      (if (string= node dest)
		  1
		  (iter (for next in (gethash node ht))
		    (sum (path-count next dest ht))))))))

(defun combine-paths (input &rest nodes)
  (iter (for (from to) on nodes)
    (while to)
    (multiplying (path-count from to input))))

(let* ((input (parse-input))
       (part-1 (path-count "you" "out" input))
       (fft-dac (path-count "fft" "dac" input)))
  (values part-1
	  (if (plusp fft-dac)
	      (combine-paths input "svr" "fft" "dac" "out")
	      (combine-paths input "svr" "dac" "fft" "out"))))
