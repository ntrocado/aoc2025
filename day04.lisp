;;; Part 1

(defun access-p (x y map)
  (< (iter outer
       (for i from (if (zerop x) 0 -1) to (if (= 135 x) 0 1))
       (iter
	 (for j from (if (zerop y) 0 -1) to (if (= 135 y) 0 1))
	 (in outer (counting (aref map (+ x i) (+ y j))))))
     5))

(defun parse-map ()
  (let ((map (make-array '(136 136)
			 :initial-contents (uiop:read-file-lines "day04-input.txt"))))
    (iter (for i from 0 below 136)
      (iter (for j from 0 below 136)
	(setf (aref map i j) (char= (aref map i j) #\@))))
    map))

(defun count-rolls (map)
  (iter outer (for i from 0 below 136)
    (iter (for j from 0 below 136)
      (in outer (counting (and (aref map i j) (access-p i j map)))))))

(count-rolls (parse-map))

;;; Part 2

(defun update-map (map)
  (let ((new-map (alexandria:copy-array map)))
    (values
     (iter outer (for i from 0 below 136)
       (iter (for j from 0 below 136)
	 (in outer (when (aref map i j)
		     (counting (not (setf (aref new-map i j)
					  (not (access-p i j map)))))))))
     new-map)))

(iter
  (for (values rolls map) first (values 0 (parse-map)) then (update-map map))
  (summing rolls)
  (until (and (not (first-iteration-p)) (zerop rolls))))
