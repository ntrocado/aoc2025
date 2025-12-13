;;; Part 1

(defun parse-buttons (s)
  (let ((str (ppcre:regex-replace-all "," s " ")))
    (iter (for (values obj pos)
	       initially (read-from-string str)
	       then (read-from-string str nil nil :start pos))
      (while obj)
      (collect obj))))

(defun parse-lights (s)
  (make-array (length s)
	      :element-type 'bit
	      :initial-contents (map 'list (lambda (ch) (if (char= ch #\.) 0 1)) s)))

(defun parse-joltage (s)
  (mapcar #'parse-integer (uiop:split-string s :separator '(#\,))))

(defun parse-machine (str)
  (ppcre:register-groups-bind ((#'parse-lights lights)
			       (#'parse-buttons buttons)
			       (#'parse-joltage joltage))
      ("\\[(.*)\\] (\\(.*\\)) {(.*)}" str)
    (values lights buttons joltage)))

(defun button-to-bit-array (button len)
  (let ((bit-array (make-array len :element-type 'bit)))
    (mapc (lambda (x) (setf (aref bit-array x) 1)) button)
    bit-array))

(defun combine-buttons (button1 button2)
  (bit-xor button1 button2))

;;; Norvig's queues

(defstruct q front end size elements)

(defun make-queue (&optional (size 20))
  (make-q :front (- size 1) :end (- size 1) :size size
	  :elements (make-sequence 'simple-vector size)))

(defun queue-elements (q)
  (do ((i (1+ (q-end q)) (1+ i))
       (result nil))
      ((> i (q-front q)) result)
    (push (svref (q-elements q) i) result)))

(defun empty-queue-p (q) (= (q-front q) (q-end q)))

(defun queue-front (q) (svref (q-elements q) (q-front q)))

(defun dequeue (q)
  (prog1 (svref (q-elements q) (q-front q)) (decf (q-front q))))

(defun enqueue (q item)
  (setf (svref (q-elements q) (q-end q)) item)
  (when (minusp (decf (q-end q))) (shift-queue q)))

(defun shift-queue (q)
  (let* ((elements (q-elements q))
	 (new elements))
    (when (> (q-front q) (/ (q-size q) 2))
      (setq new (make-sequence 'simple-vector (* 2 (q-size q))))
      (setf (q-elements q) new)
      (setf (q-size q) (* 2 (q-size q))))
    (setf (q-end q) (- (q-size q) 2 (q-front q)))
    (replace new elements :start1 (1+ (q-end q)))
    (setf (q-front q) (1- (q-size q)))))

;;;

(defun bfs (buttons goal-lights)
  (let ((visited (make-hash-table :test 'equal))
        (queue (make-queue 1000))
	(initial-state (make-array (length goal-lights) 
				   :element-type 'bit )))
    
    (enqueue queue (cons initial-state 0))
    (setf (gethash initial-state visited) t)
    
    (iter (while (not (empty-queue-p queue)))
      (for (node . presses) = (dequeue queue))
      (when (equalp goal-lights node)
        (return-from bfs presses))
      
      (iter (for b in buttons)
        (for combined = (combine-buttons node b))
        (unless (gethash combined visited)
          (setf (gethash combined visited) t)
          (enqueue queue (cons combined (1+ presses))))))))

;;; Part 2

(load "simplex-patch.lisp")

(defun solve-machine (buttons joltage)
  (let* ((obj-var (gensym))
         (var-list (iter (for i from 0 below (length buttons))
                     (collect (gensym))))
         (constraints
           (iter (for j from 0 below (length joltage))
             (collect
                 (let ((affecting-buttons
                         (iter (for button in buttons)
                           (for var in var-list)
                           (when (member j button)
                             (collect var)))))
                   `(= (+ ,@affecting-buttons) ,(nth j joltage))))))
         (solution (linear-programming:solve-problem
                    (linear-programming:parse-linear-problem
                     `(min (= ,obj-var (+ ,@var-list)))
                     (append constraints `((integer ,@var-list)))))))
    (linear-programming:solution-variable solution obj-var)))

;;; Solve both parts

(iter (for l in-file "day10-input.txt" using #'read-line)
  (for (values lights buttons joltage) = (parse-machine l))
  (for button-bit-arrays = (mapcar (lambda (x) (button-to-bit-array x (length lights))) 
				   buttons))
  (sum (bfs button-bit-arrays lights) into part-1)
  (sum (solve-machine buttons joltage) into part-2)
  (finally (return (values part-1 part-2))))
