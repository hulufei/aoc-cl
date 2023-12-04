(defpackage :2023/DAY3
  (:export #:part1
           #:part2))

(in-package :2023/DAY3)

(defun symbol-char-p (c)
  (not (or (char= c #\.)
            (digit-char-p c))))

(defun make-arr ()
  (let* ((lines (utils:get-input-lines 2023 3))
         (rows (length lines))
         (cols (length (first lines))))
    (make-array (list rows cols)
                :INITIAL-CONTENTS lines)))

(defun parse-numbers (arr)
  (destructuring-bind (n m) (array-dimensions arr)
    (loop for i from 0 below n
          for digits = nil
          with numbers = nil
          do
          (loop for j from 0 below m
                for c = (aref arr i j)
                if (digit-char-p c) do
                (push c digits)
                else do
                (unless (null digits)
                  (push (list i (nreverse digits) j)
                        numbers))
                (setf digits nil)
                finally ; handle digits collect to the end of a line
                (unless (null digits)
                  (push (list i (nreverse digits) j)
                        numbers))
                )
          finally (return numbers))))

(defun surround-points (arr row i j)
  "Surround points from i to j"
  (destructuring-bind (rows cols) (array-dimensions arr)
    (remove-if 
      #'(lambda (point)
          (let ((x (car point))
                (y (cdr point)))
            (or (< x 0)
                (< y 0)
                (= x rows)
                (= y cols))))
      (loop for col from (1- i) to (1+ j)
        with left = (cons row col)
        with right = (cons row (1+ j))
        append (list (cons (1- row) col)
                     (cons (1+ row) col)) into borders
        finally 
        (return (cons left (cons right borders))))))
  )

(defun part-number-p (arr row digits stopcol)
  "Return part number or nil"
  (loop for (x . y) in (surround-points 
                         arr
                         row 
                         (- stopcol (length digits))
                         (1- stopcol))
        for c = (aref arr x y)
        when (symbol-char-p c)
        return (parse-integer 
                 (concatenate 'string digits))))

(defun part1 ()
  (loop with arr = (make-arr)
        for check-parts in (parse-numbers arr)
        for part-number = (apply #'part-number-p arr check-parts)
        unless (null part-number)
        sum part-number))

(defun part2 ()
  (defparameter gears nil)
  
  (flet ((group-gear (arr row digits stopcol)
           (loop for (x . y) in (surround-points 
                                  arr
                                  row 
                                  (- stopcol (length digits))
                                  (1- stopcol))
                 for c = (aref arr x y)
                 when (char= #\* c) do
                 (let ((number (parse-integer 
                                 (concatenate 'string digits))))
                   (if #1=(assoc (cons x y) gears :test #'equal)
                       (push number (cdr #1#))
                       (push (list (cons x y) number) gears)))
                 )))
    (loop with arr = (make-arr)
          for check-parts in (parse-numbers arr)
          do (apply #'group-gear arr check-parts)))
  
  (loop for (nil . numbers) in gears
        when (= (length numbers) 2)
        sum (apply #'* numbers)))

;; (part2)
