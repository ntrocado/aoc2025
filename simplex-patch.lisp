;;; Patch for linear-programming/simplex
;;; Fixes issue with redundant/degenerate constraints causing
;;; "Artificial variable still in basis and cannot be replaced" error

(in-package :linear-programming/simplex)

(defun n-solve-tableau (tableau)
  "A non-consing version of solve-tableau with better handling of degenerate cases."
  (cond
    ((listp tableau)
     (let ((solved-art-tab (n-solve-tableau (first tableau)))
           (main-tab (second tableau)))
       (unless (fp= 0 (tableau-objective-value solved-art-tab)
                    (tableau-fp-tolerance-factor solved-art-tab))
         (error 'infeasible-problem-error))

       (let ((main-matrix (tableau-matrix main-tab))
             (art-matrix (tableau-matrix solved-art-tab))
             (art-basis (tableau-basis-columns solved-art-tab))
             (num-vars (tableau-var-count main-tab))
             (num-art-vars (tableau-var-count solved-art-tab))
             (num-constraints (tableau-constraint-count main-tab)))

         ;; Remove artificial variables from basis
         (iter (for basis-col in-vector (tableau-basis-columns solved-art-tab))
               (for i from 0)
           (when (>= basis-col num-vars)
             (unless (fp= 0 (aref art-matrix i num-art-vars)
                         (tableau-fp-tolerance-factor solved-art-tab))
               (error (format nil "Artificial variable ~S still non-zero" basis-col)))
             
             (let ((new-col -1))
               ;; Find non-basis variable with nonzero coefficient
               (iter (for j from 0 below num-vars)
                 (when (and (not (fp= 0 (aref art-matrix i j)
                                     (/ (tableau-fp-tolerance-factor solved-art-tab) 2)))
                            (not (find j art-basis)))
                   (setf new-col j)
                   (return)))
               
               (if (= new-col -1)
                   ;; Handle redundant constraint case
                   (let ((row-is-zero t))
                     (iter (for j from 0 below num-vars)
                       (unless (fp= 0 (aref art-matrix i j)
                                   (/ (tableau-fp-tolerance-factor solved-art-tab) 2))
                         (setf row-is-zero nil)
                         (return)))
                     (if row-is-zero
                         ;; Redundant constraint - use any non-basis variable
                         (progn
                           (iter (for j from 0 below num-vars)
                             (unless (find j art-basis)
                               (setf new-col j)
                               (return)))
                           (when (= new-col -1)
                             (setf new-col 0))
                           (when (not (fp= 0 (aref art-matrix i new-col)
                                          (/ (tableau-fp-tolerance-factor solved-art-tab) 2)))
                             (n-pivot-row solved-art-tab new-col i)))
                         (error "Artificial variable still in basis and cannot be replaced")))
                   (n-pivot-row solved-art-tab new-col i)))))

         ;; Copy coefficients and RHS to main tableau
         (iter (for row from 0 below num-constraints)
           (iter (for col from 0 below num-vars)
             (setf (aref main-matrix row col) (aref art-matrix row col)))
           (setf (aref main-matrix row num-vars)
                 (aref art-matrix row num-art-vars)))

         ;; Update basis and objective row
         (iter (for basis-col in-vector art-basis)
               (for i from 0)
           (when (< basis-col num-vars)
             (setf (aref (tableau-basis-columns main-tab) i) basis-col)
             (let ((scale (aref main-matrix num-constraints basis-col)))
               (when (not (fp= 0 scale (/ (tableau-fp-tolerance-factor main-tab) 2)))
                 (iter (for col from 0 to num-vars)
                   (decf (aref main-matrix num-constraints col)
                         (* scale (aref main-matrix i col)))))))))
       (n-solve-tableau main-tab)))
    (t
     (check-type tableau tableau)
     (iter (for entering-column = (find-entering-column tableau))
           (while entering-column)
       (let ((pivoting-row (find-pivoting-row tableau entering-column)))
         (unless pivoting-row
           (error 'unbounded-problem-error))
         (n-pivot-row tableau entering-column pivoting-row)))
     tableau)))
