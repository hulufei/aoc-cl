(defpackage :2023/DAY9
  (:use :cl))

(in-package :2023/DAY9)

(defun extrapolate (inputs &key backwards)
  (cond ((or (null inputs)
             (every #'zerop inputs)) 0)
        (backwards (- (car inputs)
                       (extrapolate 
                         (next-diffs inputs)
                         :backwards backwards)))
        (t (+ (car (last inputs))
              (extrapolate 
                (next-diffs inputs)
                :backwards backwards)))))

(defun next-diffs (inputs)
  (loop
    for lst = inputs then (rest lst)
    for fst = (first lst)
    for snd = (second lst)
    while snd
    collect (- snd fst)))

(assert (equal (next-diffs nil) nil))
(assert (equal (next-diffs '(1)) nil))
(assert (equal (next-diffs '(1 2)) '(1)))
(assert (equal (next-diffs '(1 2 4)) '(1 2)))
(assert
  (equal (next-diffs '(0 3 6 9 12 15)) '(3 3 3 3 3)))
(assert
  (equal (extrapolate '(0 3 6 9 12 15)) 18))
(assert
  (equal (extrapolate '(1   3   6  10  15  21)) 28))
(assert
  (equal (extrapolate '(10  13  16  21  30  45)) 68))

(defun solve (&key backwards)
  (reduce
    #'+
    (mapcar
      #'(lambda (inputs)
          (extrapolate inputs :BACKWARDS backwards))
      (mapcar 
        #'utils:parse-integer-in-line
        (utils:read-lines 2023 9)))))

(defun part1 ()
  (solve))

(assert (equal (part1) 2008960228))

(assert
  (equal (extrapolate
           '(10  13  16  21  30  45)
           :BACKWARDS t)
         5))

(defun part2 ()
  (solve :BACKWARDS t))

(assert (equal (part2) 1097))
