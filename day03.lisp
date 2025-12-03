;;; Part 1

(defun remove-once (char str)
  (ppcre:regex-replace char str ""))

(defun max-jolt (str)
  (flet ((largest (str &key (start 0))
	   (reduce #'max str :key #'char-int :start start)))
    (let* ((n1 (largest str))
	   (pos1 (position (code-char n1) str))
	   (n2 (largest (remove-once (code-char n1) str)
			:start (mod pos1 (1- (length str)))))
	   (pos2 (position (code-char n2) str :from-end t)))
      (parse-integer
       (if (< pos1 pos2)
	   (format nil "~d~d" (code-char n1) (code-char n2))
	   (format nil "~d~d" (code-char n2) (code-char n1)))))))

(with-open-file (in "day03-input.txt")
  (loop :for l := (read-line in nil)
	:while l
	:sum (max-jolt l)))

;;; Part 2

(defun largest (str &optional (limit 11))
  (let* ((ascii (reduce #'max str :key #'char-int :end (- (length str) limit)))
	 (pos (position (code-char ascii) str))
	 (new-str (subseq (remove-once (code-char ascii) str)
			  (mod pos (length str)))))
    (values ascii pos new-str)))

(defun max-jolt (str)
  (parse-integer
   (coerce (loop :for i :from 11 :downto 0
		 :for (ascii pos str2) := (multiple-value-list (largest str))
		   :then (multiple-value-list (largest str2 i))
		 :collect (code-char ascii))
	   'string)))
