(defun solve (part)
  (let* ((input (uiop:read-file-lines "day07-input.txt"))
         (beams (make-array (length (first input))
                            :initial-element 0))
         (splits 0))
    (iter
      (initially (incf (aref beams (position #\S (first input)))))
      (for line in (rest input))
      (iter
        (for ch in-string line)
        (for i index-of-string line)
        (for b = (aref beams i))
        (when (and (char= ch #\^) (plusp b))
          (incf splits)
          (incf (aref beams (1- i)) b)
          (incf (aref beams (1+ i)) b)
          (setf (aref beams i) 0)))
      (finally (return (if (= part 1)
                           splits
                           (reduce #'+ beams)))))))

(mapcar #'solve '(1 2))
